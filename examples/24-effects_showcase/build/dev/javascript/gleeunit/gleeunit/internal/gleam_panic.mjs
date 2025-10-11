import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import { from_dynamic } from "./gleeunit_gleam_panic_ffi.mjs";

export { from_dynamic };

export class GleamPanic extends $CustomType {
  constructor(message, file, module, function$, line, kind) {
    super();
    this.message = message;
    this.file = file;
    this.module = module;
    this.function = function$;
    this.line = line;
    this.kind = kind;
  }
}

export class Todo extends $CustomType {}

export class Panic extends $CustomType {}

export class LetAssert extends $CustomType {
  constructor(start, end, pattern_start, pattern_end, value) {
    super();
    this.start = start;
    this.end = end;
    this.pattern_start = pattern_start;
    this.pattern_end = pattern_end;
    this.value = value;
  }
}

export class Assert extends $CustomType {
  constructor(start, end, expression_start, kind) {
    super();
    this.start = start;
    this.end = end;
    this.expression_start = expression_start;
    this.kind = kind;
  }
}

export class BinaryOperator extends $CustomType {
  constructor(operator, left, right) {
    super();
    this.operator = operator;
    this.left = left;
    this.right = right;
  }
}

export class FunctionCall extends $CustomType {
  constructor(arguments$) {
    super();
    this.arguments = arguments$;
  }
}

export class OtherExpression extends $CustomType {
  constructor(expression) {
    super();
    this.expression = expression;
  }
}

export class AssertedExpression extends $CustomType {
  constructor(start, end, kind) {
    super();
    this.start = start;
    this.end = end;
    this.kind = kind;
  }
}

export class Literal extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class Expression extends $CustomType {
  constructor(value) {
    super();
    this.value = value;
  }
}

export class Unevaluated extends $CustomType {}
