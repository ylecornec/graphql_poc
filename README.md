This repo contains a proof of concept of a graphql server built in a modular way,
by defining a `Gql` module next to each type used by the graphql server.

In addition to the graphql schema, these modules provide:

- A `query` type that can be used to build graphql queries (which may contains arguments).
  For now there is only one such type but there probably should be others for mutations and subscriptions.

- A `build_query` function to build the query.
(For now it generates a string, but it should likely directly return a `json` query as well as a `variables` json object).

- A `response_of_json` object that will help parse the result of a query.

  In general the client must be careful to only access the fields that are requested in the query, but there is already quite a bit of type safety:
  - If the field is present in the response it will be parsed into the correct type.
  - Some fields may directly be parsed into a structure if they correspond to a query defined server side (often time making use of the `field_derivers` mechanism). This is the case of the `GetAll` query used in the [client.ml](client.ml) file, and defined in the [lib/address.ml](lib/address.ml) file.
