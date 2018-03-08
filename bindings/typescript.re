module Internal = {
  module ScriptTarget = {
    type t;
    [@bs.module "typescript"] [@bs.scope "ScriptTarget"]
    external es2015 : t = "ES2015";
  };
  module SyntaxKind = {
    type t = int;
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external endOfFileToken : t = "EndOfFileToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external singleLineCommentTrivia : t = "SingleLineCommentTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external multiLineCommentTrivia : t = "MultiLineCommentTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external newLineTrivia : t = "NewLineTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external whitespaceTrivia : t = "WhitespaceTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external shebangTrivia : t = "ShebangTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external conflictMarkerTrivia : t = "ConflictMarkerTrivia";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external numericLiteral : t = "NumericLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external stringLiteral : t = "StringLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxText : t = "JsxText";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxTextAllWhiteSpaces : t = "JsxTextAllWhiteSpaces";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external regularExpressionLiteral : t = "RegularExpressionLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external noSubstitutionTemplateLiteral : t =
      "NoSubstitutionTemplateLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external templateHead : t = "TemplateHead";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external templateMiddle : t = "TemplateMiddle";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external templateTail : t = "TemplateTail";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external openBraceToken : t = "OpenBraceToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external closeBraceToken : t = "CloseBraceToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external openParenToken : t = "OpenParenToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external closeParenToken : t = "CloseParenToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external openBracketToken : t = "OpenBracketToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external closeBracketToken : t = "CloseBracketToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external dotToken : t = "DotToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external dotDotDotToken : t = "DotDotDotToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external semicolonToken : t = "SemicolonToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external commaToken : t = "CommaToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external lessThanToken : t = "LessThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external lessThanSlashToken : t = "LessThanSlashToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanToken : t = "GreaterThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external lessThanEqualsToken : t = "LessThanEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanEqualsToken : t = "GreaterThanEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external equalsEqualsToken : t = "EqualsEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exclamationEqualsToken : t = "ExclamationEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external equalsEqualsEqualsToken : t = "EqualsEqualsEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exclamationEqualsEqualsToken : t = "ExclamationEqualsEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external equalsGreaterThanToken : t = "EqualsGreaterThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external plusToken : t = "PlusToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external minusToken : t = "MinusToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asteriskToken : t = "AsteriskToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asteriskAsteriskToken : t = "AsteriskAsteriskToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external slashToken : t = "SlashToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external percentToken : t = "PercentToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external plusPlusToken : t = "PlusPlusToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external minusMinusToken : t = "MinusMinusToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external lessThanLessThanToken : t = "LessThanLessThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanGreaterThanToken : t = "GreaterThanGreaterThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanGreaterThanGreaterThanToken : t =
      "GreaterThanGreaterThanGreaterThanToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external ampersandToken : t = "AmpersandToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external barToken : t = "BarToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external caretToken : t = "CaretToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exclamationToken : t = "ExclamationToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external tildeToken : t = "TildeToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external ampersandAmpersandToken : t = "AmpersandAmpersandToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external barBarToken : t = "BarBarToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external questionToken : t = "QuestionToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external colonToken : t = "ColonToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external atToken : t = "AtToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external equalsToken : t = "EqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external plusEqualsToken : t = "PlusEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external minusEqualsToken : t = "MinusEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asteriskEqualsToken : t = "AsteriskEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asteriskAsteriskEqualsToken : t = "AsteriskAsteriskEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external slashEqualsToken : t = "SlashEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external percentEqualsToken : t = "PercentEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external lessThanLessThanEqualsToken : t = "LessThanLessThanEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanGreaterThanEqualsToken : t =
      "GreaterThanGreaterThanEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external greaterThanGreaterThanGreaterThanEqualsToken : t =
      "GreaterThanGreaterThanGreaterThanEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external ampersandEqualsToken : t = "AmpersandEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external barEqualsToken : t = "BarEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external caretEqualsToken : t = "CaretEqualsToken";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external identifier : t = "Identifier";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external breakKeyword : t = "BreakKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external caseKeyword : t = "CaseKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external catchKeyword : t = "CatchKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external classKeyword : t = "ClassKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external constKeyword : t = "ConstKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external continueKeyword : t = "ContinueKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external debuggerKeyword : t = "DebuggerKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external defaultKeyword : t = "DefaultKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external deleteKeyword : t = "DeleteKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external doKeyword : t = "DoKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external elseKeyword : t = "ElseKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external enumKeyword : t = "EnumKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exportKeyword : t = "ExportKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external extendsKeyword : t = "ExtendsKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external falseKeyword : t = "FalseKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external finallyKeyword : t = "FinallyKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external forKeyword : t = "ForKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external functionKeyword : t = "FunctionKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external ifKeyword : t = "IfKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external importKeyword : t = "ImportKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external inKeyword : t = "InKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external instanceOfKeyword : t = "InstanceOfKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external newKeyword : t = "NewKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external nullKeyword : t = "NullKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external returnKeyword : t = "ReturnKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external superKeyword : t = "SuperKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external switchKeyword : t = "SwitchKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external thisKeyword : t = "ThisKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external throwKeyword : t = "ThrowKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external trueKeyword : t = "TrueKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external tryKeyword : t = "TryKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeOfKeyword : t = "TypeOfKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external varKeyword : t = "VarKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external voidKeyword : t = "VoidKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external whileKeyword : t = "WhileKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external withKeyword : t = "WithKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external implementsKeyword : t = "ImplementsKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external interfaceKeyword : t = "InterfaceKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external letKeyword : t = "LetKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external packageKeyword : t = "PackageKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external privateKeyword : t = "PrivateKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external protectedKeyword : t = "ProtectedKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external publicKeyword : t = "PublicKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external staticKeyword : t = "StaticKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external yieldKeyword : t = "YieldKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external abstractKeyword : t = "AbstractKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asKeyword : t = "AsKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external anyKeyword : t = "AnyKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asyncKeyword : t = "AsyncKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external awaitKeyword : t = "AwaitKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external booleanKeyword : t = "BooleanKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external constructorKeyword : t = "ConstructorKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external declareKeyword : t = "DeclareKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external getKeyword : t = "GetKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external inferKeyword : t = "InferKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external isKeyword : t = "IsKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external keyOfKeyword : t = "KeyOfKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external moduleKeyword : t = "ModuleKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external namespaceKeyword : t = "NamespaceKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external neverKeyword : t = "NeverKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external readonlyKeyword : t = "ReadonlyKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external requireKeyword : t = "RequireKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external numberKeyword : t = "NumberKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external objectKeyword : t = "ObjectKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external setKeyword : t = "SetKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external stringKeyword : t = "StringKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external symbolKeyword : t = "SymbolKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeKeyword : t = "TypeKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external undefinedKeyword : t = "UndefinedKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external uniqueKeyword : t = "UniqueKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external fromKeyword : t = "FromKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external globalKeyword : t = "GlobalKeyword";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external qualifiedName : t = "QualifiedName";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external computedPropertyName : t = "ComputedPropertyName";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeParameter : t = "TypeParameter";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external parameter : t = "Parameter";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external decorator : t = "Decorator";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external propertySignature : t = "PropertySignature";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external propertyDeclaration : t = "PropertyDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external methodSignature : t = "MethodSignature";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external methodDeclaration : t = "MethodDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external constructor : t = "Constructor";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external getAccessor : t = "GetAccessor";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external setAccessor : t = "SetAccessor";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external callSignature : t = "CallSignature";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external constructSignature : t = "ConstructSignature";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external indexSignature : t = "IndexSignature";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typePredicate : t = "TypePredicate";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeReference : t = "TypeReference";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external functionType : t = "FunctionType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external constructorType : t = "ConstructorType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeQuery : t = "TypeQuery";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeLiteral : t = "TypeLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external arrayType : t = "ArrayType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external tupleType : t = "TupleType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external unionType : t = "UnionType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external intersectionType : t = "IntersectionType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external conditionalType : t = "ConditionalType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external inferType : t = "InferType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external parenthesizedType : t = "ParenthesizedType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external thisType : t = "ThisType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeOperator : t = "TypeOperator";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external indexedAccessType : t = "IndexedAccessType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external mappedType : t = "MappedType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external literalType : t = "LiteralType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external objectBindingPattern : t = "ObjectBindingPattern";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external arrayBindingPattern : t = "ArrayBindingPattern";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external bindingElement : t = "BindingElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external arrayLiteralExpression : t = "ArrayLiteralExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external objectLiteralExpression : t = "ObjectLiteralExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external propertyAccessExpression : t = "PropertyAccessExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external elementAccessExpression : t = "ElementAccessExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external callExpression : t = "CallExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external newExpression : t = "NewExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external taggedTemplateExpression : t = "TaggedTemplateExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeAssertionExpression : t = "TypeAssertionExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external parenthesizedExpression : t = "ParenthesizedExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external functionExpression : t = "FunctionExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external arrowFunction : t = "ArrowFunction";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external deleteExpression : t = "DeleteExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeOfExpression : t = "TypeOfExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external voidExpression : t = "VoidExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external awaitExpression : t = "AwaitExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external prefixUnaryExpression : t = "PrefixUnaryExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external postfixUnaryExpression : t = "PostfixUnaryExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external binaryExpression : t = "BinaryExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external conditionalExpression : t = "ConditionalExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external templateExpression : t = "TemplateExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external yieldExpression : t = "YieldExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external spreadElement : t = "SpreadElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external classExpression : t = "ClassExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external omittedExpression : t = "OmittedExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external expressionWithTypeArguments : t = "ExpressionWithTypeArguments";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external asExpression : t = "AsExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external nonNullExpression : t = "NonNullExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external metaProperty : t = "MetaProperty";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external templateSpan : t = "TemplateSpan";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external semicolonClassElement : t = "SemicolonClassElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external block : t = "Block";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external variableStatement : t = "VariableStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external emptyStatement : t = "EmptyStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external expressionStatement : t = "ExpressionStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external ifStatement : t = "IfStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external doStatement : t = "DoStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external whileStatement : t = "WhileStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external forStatement : t = "ForStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external forInStatement : t = "ForInStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external forOfStatement : t = "ForOfStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external continueStatement : t = "ContinueStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external breakStatement : t = "BreakStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external returnStatement : t = "ReturnStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external withStatement : t = "WithStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external switchStatement : t = "SwitchStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external labeledStatement : t = "LabeledStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external throwStatement : t = "ThrowStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external tryStatement : t = "TryStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external debuggerStatement : t = "DebuggerStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external variableDeclaration : t = "VariableDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external variableDeclarationList : t = "VariableDeclarationList";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external functionDeclaration : t = "FunctionDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external classDeclaration : t = "ClassDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external interfaceDeclaration : t = "InterfaceDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external typeAliasDeclaration : t = "TypeAliasDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external enumDeclaration : t = "EnumDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external moduleDeclaration : t = "ModuleDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external moduleBlock : t = "ModuleBlock";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external caseBlock : t = "CaseBlock";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external namespaceExportDeclaration : t = "NamespaceExportDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external importEqualsDeclaration : t = "ImportEqualsDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external importDeclaration : t = "ImportDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external importClause : t = "ImportClause";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external namespaceImport : t = "NamespaceImport";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external namedImports : t = "NamedImports";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external importSpecifier : t = "ImportSpecifier";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exportAssignment : t = "ExportAssignment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exportDeclaration : t = "ExportDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external namedExports : t = "NamedExports";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external exportSpecifier : t = "ExportSpecifier";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external missingDeclaration : t = "MissingDeclaration";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external externalModuleReference : t = "ExternalModuleReference";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxElement : t = "JsxElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxSelfClosingElement : t = "JsxSelfClosingElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxOpeningElement : t = "JsxOpeningElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxClosingElement : t = "JsxClosingElement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxFragment : t = "JsxFragment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxOpeningFragment : t = "JsxOpeningFragment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxClosingFragment : t = "JsxClosingFragment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxAttribute : t = "JsxAttribute";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxAttributes : t = "JsxAttributes";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxSpreadAttribute : t = "JsxSpreadAttribute";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jsxExpression : t = "JsxExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external caseClause : t = "CaseClause";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external defaultClause : t = "DefaultClause";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external heritageClause : t = "HeritageClause";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external catchClause : t = "CatchClause";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external propertyAssignment : t = "PropertyAssignment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external shorthandPropertyAssignment : t = "ShorthandPropertyAssignment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external spreadAssignment : t = "SpreadAssignment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external enumMember : t = "EnumMember";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external sourceFile : t = "SourceFile";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external bundle : t = "Bundle";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTypeExpression : t = "JSDocTypeExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocAllType : t = "JSDocAllType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocUnknownType : t = "JSDocUnknownType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocNullableType : t = "JSDocNullableType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocNonNullableType : t = "JSDocNonNullableType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocOptionalType : t = "JSDocOptionalType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocFunctionType : t = "JSDocFunctionType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocVariadicType : t = "JSDocVariadicType";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocComment : t = "JSDocComment";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTypeLiteral : t = "JSDocTypeLiteral";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTag : t = "JSDocTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocAugmentsTag : t = "JSDocAugmentsTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocClassTag : t = "JSDocClassTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocParameterTag : t = "JSDocParameterTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocReturnTag : t = "JSDocReturnTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTypeTag : t = "JSDocTypeTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTemplateTag : t = "JSDocTemplateTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocTypedefTag : t = "JSDocTypedefTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external jSDocPropertyTag : t = "JSDocPropertyTag";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external syntaxList : t = "SyntaxList";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external notEmittedStatement : t = "NotEmittedStatement";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external partiallyEmittedExpression : t = "PartiallyEmittedExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external commaListExpression : t = "CommaListExpression";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external mergeDeclarationMarker : t = "MergeDeclarationMarker";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external endOfDeclarationMarker : t = "EndOfDeclarationMarker";
    [@bs.module "typescript"] [@bs.scope "SyntaxKind"]
    external count : t = "Count";
  };
  type node = {. "kind": SyntaxKind.t};
  [@bs.module "typescript"]
  external createSourceFile :
    (string, string, ScriptTarget.t, Js.boolean) => node =
    "";
};

type parseDiagnostic = {
  start: int,
  messageText: string,
  category: int,
  length: int,
  code: int
};

type keyword = {
  pos: int,
  end_: int
}
and identifier = {
  pos: int,
  end_: int,
  escapedText: string,
  text: string
}
and functionDeclaration = {
  pos: int,
  end_: int,
  modifiers: array(node),
  name: node,
  typeParameters: array(node),
  parameters: array(node),
  type_: node
}
and interfaceDeclaration = {
  pos: int,
  end_: int,
  modifiers: array(node),
  name: node,
  typeParameters: array(node),
  members: array(node)
}
and propertySignature = {
  pos: int,
  end_: int,
  modifiers: array(node),
  name: node,
  questionToken: option(node),
  type_: node
}
and questionToken = {
  pos: int,
  end_: int
}
and typeAliasDeclaration = {
  pos: int,
  end_: int,
  name: node,
  typeParameters: array(node),
  type_: node
}
and typeLiteral = {
  pos: int,
  end_: int,
  members: array(node)
}
and sourceFile = {
  pos: int,
  end_: int,
  text: string,
  languageVersion: int,
  fileName: string,
  languageVariant: int,
  isDeclarationFile: bool,
  statements: array(node),
  nodeCount: int,
  identifierCount: int,
  parseDiagnostics: array(parseDiagnostic)
}
and parameter = {
  pos: int,
  end_: int,
  dotDotDotToken: option(node),
  name: node,
  questionToken: option(node),
  type_: node
}
and typeParameter = {
  pos: int,
  end_: int,
  name: node
}
and typeReference = {
  pos: int,
  end_: int,
  typeName: node
}
and node =
  | DeclareKeyword(keyword)
  | ExportKeyword(keyword)
  | StringKeyword(keyword)
  | NumberKeyword(keyword)
  | Identifier(identifier)
  | FunctionDeclaration(functionDeclaration)
  | InterfaceDeclaration(interfaceDeclaration)
  | PropertySignature(propertySignature)
  | QuestionToken(questionToken)
  | TypeAliasDeclaration(typeAliasDeclaration)
  | TypeLiteral(typeLiteral)
  | SourceFile(sourceFile)
  | Parameter(parameter)
  | TypeParameter(typeParameter)
  | TypeReference(typeReference)
  | Unknown(int);

module Decoder = {
  let parseDiagnostic = json =>
    Json.Decode.{
      start: json |> field("start", int),
      messageText: json |> field("messageText", string),
      category: json |> field("category", int),
      code: json |> field("code", int),
      length: json |> field("length", int)
    };
  external nodeToJson : Internal.node => Js.Json.t = "%identity";
  let rec decoders = [
    (Internal.SyntaxKind.declareKeyword, declareKeyword),
    (Internal.SyntaxKind.exportKeyword, exportKeyword),
    (Internal.SyntaxKind.numberKeyword, numberKeyword),
    (Internal.SyntaxKind.stringKeyword, stringKeyword),
    (Internal.SyntaxKind.identifier, identifier),
    (Internal.SyntaxKind.functionDeclaration, functionDeclaration),
    (Internal.SyntaxKind.interfaceDeclaration, interfaceDeclaration),
    (Internal.SyntaxKind.typeAliasDeclaration, typeAliasDeclaration),
    (Internal.SyntaxKind.typeLiteral, typeLiteral),
    (Internal.SyntaxKind.propertySignature, propertySignature),
    (Internal.SyntaxKind.questionToken, questionToken),
    (Internal.SyntaxKind.sourceFile, sourceFile),
    (Internal.SyntaxKind.parameter, parameter),
    (Internal.SyntaxKind.typeParameter, typeParameter),
    (Internal.SyntaxKind.typeReference, typeReference)
  ]
  and node = json => {
    let syntaxKind = json |> Json.Decode.field("kind", Json.Decode.int);
    let decoder =
      try (List.assoc(syntaxKind, decoders)) {
      | _ => unknown
      };
    decoder(json);
  }
  and declareKeyword = json =>
    DeclareKeyword(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int)
      }
    )
  and exportKeyword = json =>
    ExportKeyword(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int)
      }
    )
  and numberKeyword = json =>
    NumberKeyword(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int)
      }
    )
  and stringKeyword = json =>
    StringKeyword(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int)
      }
    )
  and identifier = json =>
    Identifier(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        escapedText: json |> field("escapedText", string),
        text: json |> field("text", string)
      }
    )
  and functionDeclaration = json =>
    FunctionDeclaration(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        modifiers: json |> withDefault([||], field("modifiers", array(node))),
        name: json |> field("name", node),
        typeParameters:
          json |> withDefault([||], field("typeParameters", array(node))),
        parameters: json |> field("parameters", array(node)),
        type_: json |> field("type", node)
      }
    )
  and interfaceDeclaration = json =>
    InterfaceDeclaration(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        modifiers: json |> withDefault([||], field("modifiers", array(node))),
        name: json |> field("name", node),
        members: json |> field("members", array(node)),
        typeParameters:
          json |> withDefault([||], field("typeParameters", array(node)))
      }
    )
  and propertySignature = json =>
    PropertySignature(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        modifiers: json |> withDefault([||], field("modifiers", array(node))),
        name: json |> field("name", node),
        type_: json |> field("type", node),
        questionToken:
          json |> Json.Decode.optional(field("questionToken", node))
      }
    )
  and questionToken = json =>
    QuestionToken(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int)
      }
    )
  and typeAliasDeclaration = json =>
    TypeAliasDeclaration(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        name: json |> field("name", node),
        type_: json |> field("type", node),
        typeParameters:
          json |> withDefault([||], field("typeParameters", array(node)))
      }
    )
  and typeLiteral = json =>
    TypeLiteral(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        members: json |> field("members", array(node))
      }
    )
  and sourceFile = json =>
    SourceFile(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        text: json |> field("text", string),
        languageVersion: json |> field("languageVersion", int),
        fileName: json |> field("fileName", string),
        languageVariant: json |> field("languageVariant", int),
        isDeclarationFile: json |> field("isDeclarationFile", bool),
        statements: json |> field("statements", array(node)),
        nodeCount: json |> field("nodeCount", int),
        identifierCount: json |> field("identifierCount", int),
        parseDiagnostics:
          json |> field("parseDiagnostics", array(parseDiagnostic))
      }
    )
  and parameter = json =>
    Parameter(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        dotDotDotToken: None,
        name: json |> field("name", node),
        questionToken: None,
        type_: json |> field("type", node)
      }
    )
  and typeParameter = json =>
    TypeParameter(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        name: json |> field("name", node)
      }
    )
  and typeReference = json =>
    TypeReference(
      Json.Decode.{
        pos: json |> field("pos", int),
        end_: json |> field("end", int),
        typeName: json |> field("typeName", node)
      }
    )
  and unknown = json => {
    let kind = Json.Decode.field("kind", Json.Decode.int, json);
    Unknown(kind);
  };
  let decode = tsNode => {
    let json = nodeToJson(tsNode);
    node(json);
  };
};

let parse = (fileName: string, source: string) => {
  let sourceFile =
    Internal.createSourceFile(
      fileName,
      source,
      Internal.ScriptTarget.es2015,
      Js.false_
    );
  Decoder.decode(sourceFile);
};
