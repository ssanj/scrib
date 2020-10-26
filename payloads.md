# View

## Read cached notes on init from Session:

```json
{
  "scrib.view": [
                  {"noteText":"note1","noteId":1,"noteVersion":1}
                ]
}
```

## Write note to edit

```json
{
  "eventType": "storage_action",
  "storage" : {
      "storageType": "local",
      "storageKey": "scrib.edit",
      "storageAction": "save",
      "data": {
        "noteText": "A completely  new note\n\nIn the age of versions!",
        "noteId": 45,
        "noteVersion": 1
      }
  },
  "responseKey": "NoteSavedToLocalStorage"
}
```

## Render selected note

```json
{
  "eventType": "markdown_action",
  "markdown_action" : {
      "elementId": "markdown-view",
      "data": "markdown text"
  }
}
```

## Read from Save on init

```json
{
  "eventType": "save_message",
  "note": {
    "noteText": "A completely  new note\n\nIn the age of versions!",
    "noteId": 45,
    "noteVersion": 1
  }
}
```

returned to the Save in:

```json
{
  "note": {
    "noteText": "A completely  new note\n\nIn the age of versions!",
    "noteId": 45,
    "noteVersion": 1
  },
  "apiKey": "some api key"
}
```


## Saving from Save

### Without id

```json
{
  "eventType": "save_message",
  "note": {
    "noteText": "# another newie\n\nboo!",
    "noteId": null,
    "noteVersion": null
  }
}
```

### With id

```json
{
  "eventType": "save_message",
  "note": {
    "noteText": "# another newie\n\nboo!",
    "noteId": 46,
    "noteVersion": 1
  }
}
```

# scrib.view (from sessionStorage)

## used by the View in init

```json
[
  {
    "noteText": "A completely  new note\n\nIn the age of versions!",
    "noteId": 45,
    "noteVersion": 1
  },
  {
    "noteText": "from the cmd part 2-2\n\n3 three four and five",
    "noteId": 44,
    "noteVersion": 5
  }
]
```

returned to the view in:

```json
{
  "notes": [
  {
    "noteText": "A completely  new note\n\nIn the age of versions!",
    "noteId": 45,
    "noteVersion": 1
  },
  {
    "noteText": "from the cmd part 2-2\n\n3 three four and five",
    "noteId": 44,
    "noteVersion": 5
  }
  ],
  "apiKey": "somekey"
}
```

# Slate

## Save a new note

```json
  {
    "noteText": "A completely  new note\n\nIn the age of versions!",
  }
```

## Save a note with id

```json
  {
    "noteText": "A completely  new note\n\nIn the age of versions!",
    "noteId": 45,
    "noteVersion": 1
  }
```


## Result of successful save/edit

```json
{
  "noteId": 45,
  "noteVersion": 2
}
```

## Result of failed save/edit

```json
{
  "errorId": 1002,
  "errorMessage": "There's a different version of this note on the server. Refresh and try again"
}
```