open Migrate_parsetree;
open Ast_408;
open Ast_mapper;
open Parsetree;
open Ast_helper;

module Utils = {
  let annotationName = "recoil";
  let getAttributeByName = (attributes: list(attribute), name) => {
    let filtered =
      attributes |> List.filter(({attr_name: {txt, _}, _}) => txt == name);

    switch (filtered) {
    | [] => Ok(None)
    | [attribute] => Ok(Some(attribute))
    | _ => Error("Too many occurrences of \"" ++ name ++ "\" attribute")
    };
  };

  type generatorSettings = {recoil: bool};
  let getSettingsFromAttributes = (attributes: list(attribute)) =>
    switch (getAttributeByName(attributes, annotationName)) {
    | Ok(Some(_)) => Ok(Some({recoil: true}))
    | Ok(None) => Ok(None)
    | Error(_) as e => e
    };

  let fail = (loc, message) =>
    Location.error(~loc, message) |> (v => Location.Error(v) |> raise);
};

module ValueBindings = {
  let map =
      (
        {
          ptype_name: {txt: typeName, _},
          ptype_attributes,
          ptype_loc,
          ptype_kind,
          ptype_manifest,
          _,
        },
      ) => {
    switch (Utils.getSettingsFromAttributes(ptype_attributes)) {
    // TODO: tomorrow
    | Ok(Some({recoil: true})) =>
      let recoilPat = Pat.var(Location.mknoloc(typeName ++ "_atom"));
      switch (ptype_kind, ptype_manifest) {
      | (Ptype_record(_fields), _) => [
          Vb.mk(
            recoilPat,
            Exp.fun_(Asttypes.Nolabel, None, recoilPat, [%expr Recoil.atom]),
          ),
        ]
      // core type
      | (Ptype_abstract, Some(_manifest)) => [
          Vb.mk(
            recoilPat,
            // TODO: use the real data from manifest
            Exp.apply(
              [%expr Recoil.atom],
              [
                (
                  Nolabel,
                  // TODO: put record
                  Exp.record(
                    [
                      (
                        {loc: Location.none, txt: Lident("default")},
                        [%expr ""],
                      ),
                      (
                        {loc: Location.none, txt: Lident("key")},
                        [%expr "apapun"],
                      ),
                    ],
                    None,
                  ),
                ),
              ],
            ),
          ),
        ]
      | _ => []
      };
    | Error(s) => Utils.fail(ptype_loc, s)
    | _ => []
    };
  };
};

module StructureItem = {
  let map = (mapper, structure_item) => {
    switch (structure_item.pstr_desc) {
    | Pstr_type(rec_flag, item_declarations) =>
      let value_bindings =
        item_declarations |> List.map(ValueBindings.map) |> List.concat;

      [mapper.structure_item(mapper, structure_item)]
      @ (
        List.length(value_bindings) > 0
          ? [Str.value(rec_flag, value_bindings)] : []
      );

    | _ => [mapper.structure_item(mapper, structure_item)]
    };
  };
};

module Stucture = {
  let map = (mapper, structures) => {
    structures |> List.map(StructureItem.map(mapper)) |> List.concat;
  };
};

let mapper = (_, _) => {...default_mapper, structure: Stucture.map};

let () =
  Migrate_parsetree.(
    Driver.register(~name="ppx_42", Versions.ocaml_408, mapper)
  );
