// Generated by BUCKLESCRIPT VERSION 4.0.14, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Js_option from "bs-platform/lib/es6/js_option.js";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";
import * as Caml_format from "bs-platform/lib/es6/caml_format.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as Caml_js_exceptions from "bs-platform/lib/es6/caml_js_exceptions.js";
import * as Caml_builtin_exceptions from "bs-platform/lib/es6/caml_builtin_exceptions.js";

function parseNumValue(numStr) {
  var parsed;
  try {
    parsed = Js_option.some(Caml_format.caml_int_of_string(numStr));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      parsed = undefined;
    } else {
      throw exn;
    }
  }
  if (parsed !== undefined) {
    var n = parsed;
    if (n >= 2 && n <= 10) {
      return /* Num */[n];
    } else {
      return undefined;
    }
  }
  
}

function parseValue(value) {
  switch (value) {
    case "A" : 
        return /* Ace */3;
    case "J" : 
        return /* Jack */0;
    case "K" : 
        return /* King */2;
    case "Q" : 
        return /* Queen */1;
    default:
      return parseNumValue(value);
  }
}

function parseSuit(suitStr) {
  switch (suitStr) {
    case "C" : 
        return /* Clubs */3;
    case "D" : 
        return /* Diamonds */1;
    case "H" : 
        return /* Hearts */0;
    case "S" : 
        return /* Spades */2;
    default:
      return undefined;
  }
}

function parseOrdinaryCard(cardStr) {
  var length = cardStr.length;
  var suitStr = cardStr.slice(length - 1 | 0);
  var valueStr = cardStr.slice(0, length - 1 | 0);
  var match = parseValue(valueStr);
  var match$1 = parseSuit(suitStr);
  if (match !== undefined && match$1 !== undefined) {
    return Js_option.some(/* OrdinaryCard */[
                match,
                match$1
              ]);
  }
  
}

function parseCard(cardStr) {
  if (cardStr === "J") {
    return /* Joker */0;
  } else {
    return parseOrdinaryCard(cardStr);
  }
}

var Parser = /* module */[
  /* parseNumValue */parseNumValue,
  /* parseValue */parseValue,
  /* parseSuit */parseSuit,
  /* parseOrdinaryCard */parseOrdinaryCard,
  /* parseCard */parseCard
];

function suitToString(suit) {
  switch (suit) {
    case 0 : 
        return "Hearts";
    case 1 : 
        return "Diamonds";
    case 2 : 
        return "Spades";
    case 3 : 
        return "Clubs";
    
  }
}

function numToString(num) {
  switch (num) {
    case 1 : 
        return Pervasives.failwith("this is an exception from numToString");
    case 2 : 
        return "Two";
    case 3 : 
        return "Three";
    case 4 : 
        return "Four";
    case 5 : 
        return "Five";
    case 6 : 
        return "Six";
    case 7 : 
        return "Seven";
    case 8 : 
        return "Eight";
    case 9 : 
        return "Nine";
    case 0 : 
    case 10 : 
        return "Ten";
    default:
      return Pervasives.failwith("this is an exception from numToString");
  }
}

function valueToString(value) {
  if (typeof value === "number") {
    switch (value) {
      case 0 : 
          return "Jack";
      case 1 : 
          return "Queen";
      case 2 : 
          return "King";
      case 3 : 
          return "Ace";
      
    }
  } else {
    return numToString(value[0]);
  }
}

function renderCard(card) {
  if (card) {
    return valueToString(card[0]) + (" of " + suitToString(card[1]));
  } else {
    return "Joker";
  }
}

var defaultErrorCard = "-- unknown card --";

var RenderToString = /* module */[
  /* suitToString */suitToString,
  /* numToString */numToString,
  /* valueToString */valueToString,
  /* renderCard */renderCard,
  /* defaultErrorCard */defaultErrorCard
];

function map(fn, opt) {
  if (opt !== undefined) {
    return Js_option.some(Curry._1(fn, Caml_option.valFromOption(opt)));
  }
  
}

function withDefault(defaultValue, opt) {
  if (opt !== undefined) {
    return Caml_option.valFromOption(opt);
  } else {
    return defaultValue;
  }
}

var Option = /* module */[
  /* map */map,
  /* withDefault */withDefault
];

function parseAndRender(cardStr) {
  return withDefault(defaultErrorCard, map(renderCard, parseCard(cardStr)));
}

export {
  Parser ,
  RenderToString ,
  Option ,
  parseAndRender ,
  
}
/* No side effect */
