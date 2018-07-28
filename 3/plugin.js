function camelCase(str) {
  return str
    .replace(/(?:^\w|[A-Z]|\b\w)/g, function(letter, index) {
      return index == 0 ? letter.toLowerCase() : letter.toUpperCase();
    })
    .replace(/\s+/g, "");
}

function pascalCase(str) {
  const [head, ...tail] = camelCase(str).split("");
  return [head.toUpperCase(), ...tail].join("");
}

function getNodeName(node, t) {
  if (t.isIdentifier(node.id)) {
    return node.id.name;
  }
  return null;
}

function getScopeBinding(scope, name) {
  if (scope && scope.bindings) {
    return scope.bindings[name];
  }
  return null;
}

const SUPPORTED_TYPES = [
  "TypeAlias",
  "InterfaceDeclaration",
  "FunctionDeclaration",
  "ClassDeclaration"
  // "ExportDefaultDeclaration",
  // "ExportNamedDeclaration",
];

function filterOutNodes(path) {
  return SUPPORTED_TYPES.includes(path.type);
}

module.exports = function plugin({ types: t }) {
  const visitor = {
    Program(path) {
      path.node.body = path.node.body.filter(filterOutNodes);
    },
    TypeAlias(path) {
      path.node.id.name = camelCase(path.node.id.name);
    },
    InterfaceDeclaration(path) {
      path.node.id.name = pascalCase(path.node.id.name);
    },
    ObjectTypeProperty(path) {
      if (t.isGenericTypeAnnotation(path.node.value)) {
        const name = getNodeName(path.node.value, t);
        const binding = getScopeBinding(path.scope, name);
        if (binding) {
          if (t.isUnionTypeAnnotation(binding.path.node.right)) {
            path.node.value = binding.path.node.right;
          } else if (t.isIdentifier(binding.identifier)) {
            path.node.value.id.name = binding.identifier.name;
          }
        } else {
          path.node.value.id.name = camelCase(path.node.value.id.name);
        }
      }
    },
    BlockStatement(path) {
      path.node.body = [];
    },
    ClassBody(path) {
      path.node.body = [];
    },
    FunctionDeclaration(path) {
      const scope = path.parentPath.scope;
      path.node.params.forEach(param => {
        path.traverse({
          TypeAnnotation(path) {
            if (t.isGenericTypeAnnotation(path.node.typeAnnotation)) {
              const name = getNodeName(path.node.typeAnnotation, t);
              const binding = getScopeBinding(scope, name);
              if (binding) {
                if (t.isUnionTypeAnnotation(binding.path.node.right)) {
                  path.node.typeAnnotation = binding.path.node.right;
                } else if (t.isIdentifier(binding.identifier)) {
                  path.node.typeAnnotation.id.name = binding.identifier.name;
                }
              } else {
                path.node.typeAnnotation.id.name = camelCase(
                  path.node.typeAnnotation.id.name
                );
              }
            }
          }
        });
      });
    }
  };

  return { visitor };
};
