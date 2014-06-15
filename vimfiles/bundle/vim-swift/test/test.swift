let test = "hello!"
let number = 1234
let number = 1.234
/*
   test
 */
struct Card {
  var rank: Rank
  var suit: Suit
  func simpleDescription() -> String {
    return "The \(rank.simpleDescription())
    of \(suit.simpleDescription())"
  }
}

let threeOfSpades = Card(rank: .Three, suit: .Spades)
let threeOfSpadesDescription = threeOfSpades.simpleDescription()
