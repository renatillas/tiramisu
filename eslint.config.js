import js from "@eslint/js";
import globals from "globals";

export default [
  js.configs.recommended,
  {
    files: ["**/*.{js,mjs,cjs}"],
    languageOptions: {
      globals: globals.browser
    },
    rules: {
      "no-unused-vars": ["error", {
        "argsIgnorePattern": "^_",
        "varsIgnorePattern": "^_",
        "caughtErrorsIgnorePattern": "^_"
      }],
      // Prevent global variable creation
      "no-var": "error",  // Disallow var declarations (use let/const instead)
      "no-implicit-globals": "warn",  // Warn about declarations in global scope
      "no-restricted-syntax": [
        "warn",
        {
          "selector": "AssignmentExpression[left.object.name='window']",
          "message": "Avoid creating global variables via window object"
        },
        {
          "selector": "AssignmentExpression[left.object.name='globalThis']",
          "message": "Avoid creating global variables via globalThis object"
        }
      ]
    }
  }
];
