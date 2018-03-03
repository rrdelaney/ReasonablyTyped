module ScriptTarget = {
  type t;
  [@bs.module "typescript"] [@bs.scope "ScriptTarget"]
  external es2015 : t = "ES2015";
};

module SyntaxKind = {
  type t;
  [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
  external forStatement : t = "ForStatement";
  [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
  external forInStatement : t = "ForInStatement";
  [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
  external forStatement : t = "ForStatement";
  [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
  external forStatement : t = "ForStatement";
};

type node;

[@bs.module "typescript"]
external createSourceFile :
  (string, string, ScriptTarget.t, Js.boolean) => node =
  "";
