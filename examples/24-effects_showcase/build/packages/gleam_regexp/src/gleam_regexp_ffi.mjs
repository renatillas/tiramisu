import { Error, List, Ok } from "./gleam.mjs";
import {
  CompileError as RegexCompileError,
  Match as RegexMatch,
} from "./gleam/regexp.mjs";
import { Some, None } from "../gleam_stdlib/gleam/option.mjs";

export function check(regex, string) {
  regex.lastIndex = 0;
  return regex.test(string);
}

export function compile(pattern, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive) flags += "i";
    if (options.multi_line) flags += "m";
    return new Ok(new RegExp(pattern, flags));
  } catch (error) {
    const number = (error.columnNumber || 0) | 0;
    return new Error(new RegexCompileError(error.message, number));
  }
}

export function split(regex, string) {
  return List.fromArray(
    string.split(regex).map((item) => (item === undefined ? "" : item)),
  );
}

export function scan(regex, string) {
  regex.lastIndex = 0;
  const matches = Array.from(string.matchAll(regex)).map((match) => {
    const content = match[0];
    return new RegexMatch(content, submatches(match.slice(1)));
  });
  return List.fromArray(matches);
}

export function replace(regex, original_string, replacement) {
  regex.lastIndex = 0;
  return original_string.replaceAll(regex, replacement);
}

export function match_map(regex, original_string, replacement) {
  regex.lastIndex = 0;
  let replace = (match, ...args) => {
    const hasNamedGroups = typeof args.at(-1) === "object";
    const groups = args.slice(0, hasNamedGroups ? -3 : -2);
    let regexMatch = new RegexMatch(match, submatches(groups));
    return replacement(regexMatch);
  };
  return original_string.replaceAll(regex, replace);
}

function submatches(groups) {
  const submatches = [];
  for (let n = groups.length - 1; n >= 0; n--) {
    if (groups[n]) {
      submatches[n] = new Some(groups[n]);
      continue;
    }
    if (submatches.length > 0) {
      submatches[n] = new None();
    }
  }
  return List.fromArray(submatches);
}
