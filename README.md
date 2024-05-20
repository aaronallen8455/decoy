# decoy :duck:

## Summary
Run a server that returns mock responses for use in testing. New response
configuration can be added dynamically so that test data needn't exist solely
in static files. Responses use mustache templating with access to variables
from JSON request bodies, URL parameters, and query string arguments.

## Basic usage
Decoy can used as either a standalone executable or as a Haskell library.

### Executable
The Decoy exectuable starts a mock server on the port specified by the
`DECOY_PORT` environment variable (defaults to 9000 if not specified). You can
also supply a path to a file containing rule specifications via the
`DECOY_RULES_FILE` environment variable. Multiple files can be specified using
whitespace separation.

### Library
Haskell projects can take advantage of the API for manipulating running server
instances without making network calls. See the library's haddock documentation
for details on the available functionality.

## Mock server usage
A Decoy server consists of a set of rules, each of which corresponds to an
endpoint with a specific response and further conditions that must be met for
the rule to be matched by a request. Rules can be supplied to the server by
giving it the path to a static file containing JSON specifications for rules or
by adding rules to server dynamically while it is running.

To add a rules dynamically, send a post request to the `_add-rules` endpoint
with an array of JSON specifications for the rules to be added as the request
body. The response will be a JSON array of the IDs assigned to those rules in
the order they were sent. For example, if the server is running on port 9000,
the request would be sent to `http://localhost:9000/_add-rules`. The JSON for
specifying a rule is outlined below.

Rules can be removed from a server using the `_remove-rules`
endpoint which take as the request body an array of rule IDs or a single rule
ID respectively.

The server can be reset to to its initial state by hitting the `_reset` endpoint.

## Rule specification
The [JSON schema](https://json-schema.org) for the rule specification object
can be found [here](./rule_spec.schema.json).
