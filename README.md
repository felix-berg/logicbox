#### TODO
- [ ] migrate tests to use [`Scalamock`](https://www.scalatest.org/user_guide/testing_with_mock_objects)

#### Endpoints
POST: /proof
GET: /proof/<proof-id>
GET: /proof/<proof-id>/validate
DELETE: /proof/<proof-id>

#### Endpoint for interacting with proof (executing commands)
PATCH: /proof/<proof-id>/execute
- inputs:
  - commands: json array of command objects (like below)
  - validate?
  - checkpointTimestamp (millis unix time)
- result:
```js
{
  proof: /* representation of proof */,
  validationResult: /* result of validation */
}
```
- http errors (400 type things)
  - eg uuid was taken, ...

Checkpoint is kept next to every proof id, recording when last operation was
performed. If client tries to change proof, and its given
`checkpoint_timestamp` is lower than it, backend rejects.

##### Commands
A command has
```js
{
  commandName: /* described below */,
  options: {/* described below, based on which `name` */}
}
```

Possible commands are
- "initLine" (will shit pants if uuids exist)
  - options:
    - newLineUuid
    - neighbourUuid
    - placement: (enum: "before"/"after")
- "initBox" (initialize box with a single line)
  - options:
    - newBoxUuid
    - newLineUuid
    - neighbourUuid
    - placement: (enum "before"/"after")
- "removeStep"
  - options:
    - uuid
- "updateLine"
  - options:
    - lineUuid
    - formula (null or string)
    - rule (null or string)
    - refs (null or [<uuid>])
- (maybe in future: "updateBox")

Note to self: if step removed, should still respond with proof tree with 'dangling' uuids.

##### Diagnostics
```js
{
  isValid: false,
  diagnostics: [
    {
      uuid: "safabjghqe3wgjkæagjheakl",
      violationType: "references_mismatch",
      violation: {
        explanation: "you were the chosen one!",
        refs: ["algasdhgkleghl", "dfsakjflajk"]
      }
    }, {
      uuid: "saldfjasfjalskj",
      violationType: "wrong_number_of_references",
      violation: {
        explanation: "lsadkjalskfj"
        expected: 3,
        actual: 4,
      }
    }, {
      uuid: "afaslkdhafkl",
      violationType: "reference_should_be_box",
      violation: {
        explanation: "you were the chosen one!",
        ref: "asdfaodgh42iowhf"
      }
    }, {
      uuid: "asdflaksjf32fjlkwj",
      violationType: "reference_should_be_line",
      violation: {
        explanation: "you were the chosen one!",
        ref: "asdfaodgh42iowhf"
      }
    }, {
      uuid: "asdlfjasdlsakjfoaj",
      violationType: "reference_doesnt_match_rule",
      violation: {
        explanation: "you were the chosen one!",
        ref: "asdfaodgh42iowhf"
      }
    }, {
      uuid: "asdlfajksflaskjlkjf",
      violationType: "formula_doesnt_match_reference",
      violation: {
        explanation: "bro",
        ref: "agadsjkldjfglkj"
      }
    }, {
      violationType: "formula_doesnt_match_rule",
      violation: {
        explanation: "bro"
      }
    }, {
      violationType: "miscellaneousViolation",
      violation: {
        explanation: "bro"
      }
    }
  ]
}
```

Described by TypeScript types
```ts
type UUID = string;

// Individual violation types
type ReferencesMismatchViolation = {
  explanation: string;
  refs: UUID[];
};

type WrongNumberOfReferencesViolation = {
  explanation: string;
  expected: number;
  actual: number;
};

type ReferenceShouldBeBoxViolation = {
  explanation: string;
  ref: UUID;
};

type ReferenceShouldBeLineViolation = {
  explanation: string;
  ref: UUID;
};

type ReferenceDoesntMatchRuleViolation = {
  explanation: string;
  ref: UUID;
};

type FormulaDoesntMatchReferenceViolation = {
  explanation: string;
  ref: UUID;
};

type FormulaDoesntMatchRuleViolation = {
  explanation: string;
};

type MiscellaneousViolation = {
  explanation: string;
};

// Union type for all possible violations
type Violation =
  | { violationType: "references_mismatch"; violation: ReferencesMismatchViolation }
  | { violationType: "wrong_number_of_references"; violation: WrongNumberOfReferencesViolation }
  | { violationType: "reference_should_be_box"; violation: ReferenceShouldBeBoxViolation }
  | { violationType: "reference_should_be_line"; violation: ReferenceShouldBeLineViolation }
  | { violationType: "reference_doesnt_match_rule"; violation: ReferenceDoesntMatchRuleViolation }
  | { violationType: "formula_doesnt_match_reference"; violation: FormulaDoesntMatchReferenceViolation }
  | { violationType: "formula_doesnt_match_rule"; violation: FormulaDoesntMatchRuleViolation }
  | { violationType: "miscellaneousViolation"; violation: MiscellaneousViolation };

// Diagnostic type
type Diagnostic = {
  uuid: UUID;
} & Violation;

// Main response type
type ValidationResponse = {
  isValid: boolean;
  diagnostics: Diagnostic[];
};
```
