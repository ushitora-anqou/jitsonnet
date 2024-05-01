/*
Copyright 2015 Google Inc. All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

/* This is the Jsonnet standard library, at least the parts of it that are written in Jsonnet.
 *
 * There are some native methods as well, which are defined in the interpreter and added to this
 * file.  It is never necessary to import std.jsonnet, it is embedded into the interpreter at
 * compile-time and automatically imported into all other Jsonnet programs.
 */
{

  local std = self,
  local id = function(x) x,

  local go_only_function = error 'This function is only supported in go version of jsonnet. See https://github.com/google/go-jsonnet',

  isString(v):: std.type(v) == 'string',
  isNumber(v):: std.type(v) == 'number',
  isBoolean(v):: std.type(v) == 'boolean',
  isObject(v):: std.type(v) == 'object',
  isArray(v):: std.type(v) == 'array',
  isFunction(v):: std.type(v) == 'function',

  toString(a)::
    if std.type(a) == 'string' then a else '' + a,

  substr(str, from, len)::
    assert std.isString(str) : 'substr first parameter should be a string, got ' + std.type(str);
    assert std.isNumber(from) : 'substr second parameter should be a string, got ' + std.type(from);
    assert std.isNumber(len) : 'substr third parameter should be a string, got ' + std.type(len);
    assert len >= 0 : 'substr third parameter should be greater than zero, got ' + len;
    std.join('', std.makeArray(std.max(0, std.min(len, std.length(str) - from)), function(i) str[i + from])),

  startsWith(a, b)::
    if std.length(a) < std.length(b) then
      false
    else
      std.substr(a, 0, std.length(b)) == b,

  endsWith(a, b)::
    if std.length(a) < std.length(b) then
      false
    else
      std.substr(a, std.length(a) - std.length(b), std.length(b)) == b,

  lstripChars(str, chars)::
    if std.length(str) > 0 && std.member(chars, str[0]) then
      std.lstripChars(str[1:], chars) tailstrict
    else
      str,

  rstripChars(str, chars)::
    local len = std.length(str);
    if len > 0 && std.member(chars, str[len - 1]) then
      std.rstripChars(str[:len - 1], chars) tailstrict
    else
      str,

  stripChars(str, chars)::
    std.lstripChars(std.rstripChars(str, chars), chars),

  stringChars(str)::
    std.makeArray(std.length(str), function(i) str[i]),

  local parse_nat(str, base) =
    assert base > 0 && base <= 16 : 'integer base %d invalid' % base;
    // These codepoints are in ascending order:
    local zero_code = std.codepoint('0');
    local upper_a_code = std.codepoint('A');
    local lower_a_code = std.codepoint('a');
    local addDigit(aggregate, char) =
      local code = std.codepoint(char);
      local digit = if code >= lower_a_code then
        code - lower_a_code + 10
      else if code >= upper_a_code then
        code - upper_a_code + 10
      else
        code - zero_code;
      assert digit >= 0 && digit < base : '%s is not a base %d integer' % [str, base];
      base * aggregate + digit;
    std.foldl(addDigit, std.stringChars(str), 0),

  parseInt(str)::
    assert std.isString(str) : 'Expected string, got ' + std.type(str);
    assert std.length(str) > 0 && str != '-' : 'Not an integer: "%s"' % [str];
    if str[0] == '-' then
      -parse_nat(str[1:], 10)
    else
      parse_nat(str, 10),

  parseOctal(str)::
    assert std.isString(str) : 'Expected string, got ' + std.type(str);
    assert std.length(str) > 0 : 'Not an octal number: ""';
    parse_nat(str, 8),

  parseHex(str)::
    assert std.isString(str) : 'Expected string, got ' + std.type(str);
    assert std.length(str) > 0 : 'Not hexadecimal: ""';
    parse_nat(str, 16),

  split(str, c)::
    assert std.isString(str) : 'std.split first parameter must be a String, got ' + std.type(str);
    assert std.isString(c) : 'std.split second parameter must be a String, got ' + std.type(c);
    assert std.length(c) >= 1 : 'std.split second parameter must have length 1 or greater, got ' + std.length(c);
    std.splitLimit(str, c, -1),

  splitLimit(str, c, maxsplits)::
    assert std.isString(str) : 'str.splitLimit first parameter must be a String, got ' + std.type(str);
    assert std.isString(c) : 'str.splitLimit second parameter must be a String, got ' + std.type(c);
    assert std.length(c) >= 1 : 'std.splitLimit second parameter must have length 1 or greater, got ' + std.length(c);
    assert std.isNumber(maxsplits) : 'str.splitLimit third parameter must be a Number, got ' + std.type(maxsplits);
    local strLen = std.length(str);
    local cLen = std.length(c);
    local aux(idx, ret, val) =
      if idx >= strLen then
        ret + [val]
      else if str[idx:idx + cLen:1] == c &&
              (maxsplits == -1 || std.length(ret) < maxsplits) then
        aux(idx + cLen, ret + [val], '')
      else
        aux(idx + 1, ret, val + str[idx]);
    aux(0, [], ''),

  splitLimitR(str, c, maxsplits)::
    assert std.isString(str) : 'str.splitLimitR first parameter must be a String, got ' + std.type(str);
    assert std.isString(c) : 'str.splitLimitR second parameter must be a String, got ' + std.type(c);
    assert std.length(c) >= 1 : 'std.splitLimitR second parameter must have length 1 or greater, got ' + std.length(c);
    assert std.isNumber(maxsplits) : 'str.splitLimitR third parameter must be a Number, got ' + std.type(maxsplits);
    if maxsplits == -1 then
      std.splitLimit(str, c, -1)
    else
      local revStr(str) = std.join('', std.reverse(std.stringChars(str)));
      std.map(function(e) revStr(e), std.reverse(std.splitLimit(revStr(str), revStr(c), maxsplits))),

  strReplace(str, from, to)::
    assert std.isString(str);
    assert std.isString(from);
    assert std.isString(to);
    assert from != '' : "'from' string must not be zero length.";

    // Cache for performance.
    local str_len = std.length(str);
    local from_len = std.length(from);

    // True if from is at str[i].
    local found_at(i) = str[i:i + from_len] == from;

    // Return the remainder of 'str' starting with 'start_index' where
    // all occurrences of 'from' after 'curr_index' are replaced with 'to'.
    local replace_after(start_index, curr_index, acc) =
      if curr_index > str_len then
        acc + str[start_index:curr_index]
      else if found_at(curr_index) then
        local new_index = curr_index + std.length(from);
        replace_after(new_index, new_index, acc + str[start_index:curr_index] + to) tailstrict
      else
        replace_after(start_index, curr_index + 1, acc) tailstrict;

    // if from_len==1, then we replace by splitting and rejoining the
    // string which is much faster than recursing on replace_after
    if from_len == 1 then
      std.join(to, std.split(str, from))
    else
      replace_after(0, 0, ''),

  asciiUpper(str)::
    local cp = std.codepoint;
    local up_letter(c) = if cp(c) >= 97 && cp(c) < 123 then
      std.char(cp(c) - 32)
    else
      c;
    std.join('', std.map(up_letter, std.stringChars(str))),

  asciiLower(str)::
    local cp = std.codepoint;
    local down_letter(c) = if cp(c) >= 65 && cp(c) < 91 then
      std.char(cp(c) + 32)
    else
      c;
    std.join('', std.map(down_letter, std.stringChars(str))),

  range(from, to)::
    std.makeArray(to - from + 1, function(i) i + from),

  repeat(what, count)::
    local joiner =
      if std.isString(what) then ''
      else if std.isArray(what) then []
      else error 'std.repeat first argument must be an array or a string';
    std.join(joiner, std.makeArray(count, function(i) what)),

  slice(indexable, index, end, step)::
    local invar =
      // loop invariant with defaults applied
      {
        indexable: indexable,
        index:
          if index == null
          then 0
          else
            if index < 0
            then std.max(0, std.length(indexable) + index)
            else index,
        end:
          if end == null
          then std.length(indexable)
          else
            if end < 0
            then std.length(indexable) + end
            else end,
        step:
          if step == null
          then 1
          else step,
        length: std.length(indexable),
        type: std.type(indexable),
      };
    assert invar.step >= 0 : 'got [%s:%s:%s] but negative steps are not supported' % [invar.index, invar.end, invar.step];
    assert step != 0 : 'got %s but step must be greater than 0' % step;
    assert std.isString(indexable) || std.isArray(indexable) : 'std.slice accepts a string or an array, but got: %s' % std.type(indexable);
    local build(slice, cur) =
      if cur >= invar.end || cur >= invar.length then
        slice
      else
        build(
          if invar.type == 'string' then
            slice + invar.indexable[cur]
          else
            slice + [invar.indexable[cur]],
          cur + invar.step
        ) tailstrict;
    build(if invar.type == 'string' then '' else [], invar.index),

}

