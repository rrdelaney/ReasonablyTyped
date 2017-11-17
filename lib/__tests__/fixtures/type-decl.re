type stringOptions = {. "value": string};

[@bs.module "type-decl"] external test : (~s: string, ~options: stringOptions) => string = "";