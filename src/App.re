
type card = {
  image: string,
  code: string
};

type deck = {
  deckId: string,
  remaining: int,
  cards: list(card)
};

type action =
  /* create deck */
  | CreateDeck
  | DeckCreated(deck)
  | CreateDeckFailed
  /* draw cards  */
  | DrawCards(deck)
  | CardsDrawn(deck)
  | DrawCardsFailed
  | DeckFinished(list(card));

type state =
  | CreatingDeck
  | WaitingForUser(deck)
  | DrawingCards(deck)
  | NoMoreCardsToDraw(list(card))
  | Error;


let decodeCreatedDeck = json => {
  Json.Decode.{
    deckId: json |> field("deck_id", string),
    remaining: json |> field("remaining", int),
    cards: []
  }
}

let createDeckSideEffects = self =>
  Js.Promise.(
    Fetch.fetch("https://deckofcardsapi.com/api/deck/new/shuffle/")
    |> then_(Fetch.Response.json)
    |> then_(json => decodeCreatedDeck(json) |> resolve)
    |> then_(deck => DeckCreated(deck) |> self.send |> resolve)
  );



let reducer = (action, _state) =>
  switch action {
  | CreateDeck => ReasonReact.UpdateWithSideEffects(
      CreatingDeck,
      createDeckSideEffects
    )
  | DeckCreated(deck) => ReasonReact.Update(WaitingForUser(deck))
  | CreateDeckFailed => ReasonReact.Update(Error)
  | DrawCards(stateDeck) => ReasonReact.UpdateWithSideEffects(
      DrawingCards(stateDeck),
      drawCardsSideEffects(stateDeck)
    )
  | CardsDrawn(deck) => ReasonReact.Update(WaitingForUser(deck))
  | DrawCardsFailed => ReasonReact.Update(Error)
  | DeckFinished(cards) => ReasonReact.Update(NoMoreCardsToDraw(cards))
  };
