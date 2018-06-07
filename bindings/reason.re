type ast;

[@bs.module "reason"] external parseRE : string => ast = "";
[@bs.module "reason"] external parseREI : string => ast = "";
[@bs.module "reason"] external parseML : string => ast = "";
[@bs.module "reason"] external parseMLI : string => ast = "";
[@bs.module "reason"] external printRE : ast => string = "";
[@bs.module "reason"] external printREI : ast => string = "";
[@bs.module "reason"] external printML : ast => string = "";
[@bs.module "reason"] external printMLI : ast => string = "";
