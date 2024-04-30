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

}

