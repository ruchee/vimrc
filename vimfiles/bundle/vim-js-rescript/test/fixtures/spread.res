// See: https://github.com/rescript-lang/syntax/blob/master/tests/parsing/errors/other/spread.js

let arr = [...x, ...y]
let [...arr, _] = [1, 2, 3]

let record = {...x, ...y}
let {...x, ...y} = myRecord

let myList = list{...x, ...y}
let list{...x, ...y} = myList
