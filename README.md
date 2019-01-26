_Under Construction_

# Which Bus?

[![Build Status](https://travis-ci.com/skyqrose/which-bus.svg?branch=master)](https://travis-ci.com/skyqrose/which-bus)

MBTA transit predictions for a customizable list of stops.

Intended to help you decide which route to take when you have multiple options available. For example, to help you answer the question: "Should I take the Red Line or Commuter Rail from South Station to Braintree? Commuter Rail is faster, but is there a train soon?" You could make a list that shows predictions for the Red Line, Kingston/Plymouth Line, and Middleborough/Lakeville.

Enter the route id and stop id for each stop that you want predictions at. These are the internal ids given by the API, which may be different than the human-facing names. If the stop has a parent stop (e.g. subway stops), then you must use the child stop id for the specific platform you want, not the parent id for the whole station.

The list of stops is saved as query parameters in the url, so you can bookmark useful lists.

## Technical Details

To run:
* `npm install`
* `npm start`

Built on top of the [MBTA Streaming API](https://www.mbta.com/developers/v3-api/streaming)

Written in [Elm](https://elm-lang.org/), my favorite language, with wonderful features like a compiler that totally eliminates runtime errors. It's a delight to write in, and I'd recommend trying it.

Hosted with GitHub Pages. There is no backend code for this project.

Disclaimer: I work for the MBTA doing stuff very similar to this, but this is a personal side project done on my own time.
