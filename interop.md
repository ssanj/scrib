# Interop Payloads

## Ports

The basic structure:

```json
{
    "eventType" : "name_of_event",
    "namespace" : {
        "other_field1" : "other_field_value1",
        "other_field2" : "other_field_value2",
        "data" : "your data"
    },
    "responseKey": "the response key"
    }
}
```

| Field | What is it | Example |
| ----- | ----------- | ------- |
| eventType | identifies the action to perform | "storage_action" |
| namespace | the metadata for the action | "storage" |
| data | The data used by the action | "hello from Elm" |
| other_fieldx | metadata fields required to perform the action | <code>{<br/>&nbsp;&nbsp;"storageType": "session",<br/>&nbsp;&nbsp;"storageKey": "scrib.view",<br/>&nbsp;&nbsp;"storageAction": "save",<br/>}</code>|
| responseKey | The key used to send back a response (if any). If no response is needed for the action, this field should be omitted. | "TopNotesSavedToSessionStorage" |

### Log

```json
{
    "eventType" : "log_action",
    "log" : {
        "appName": "your_app_name",
        "data": "your message to log"
    }
}
```

| Field | What is it? | Possible Values | Mandatory |
| ----- | ------------- | --------------- | --------- |
| appName | prefix to use when logging | any String | Yes |
| data | anything you want logged | any String | Yes |

### Storage

```json
{
    "eventType" : "storage_action",
    "storage" : {
        "storageType": "session",
        "storageKey": "scrib.view",
        "storageAction": "save",
        "data": "what you want to store"
    },
    "responseKey" : "the key that will have the response in subs"
}
```

 | Field | What is it? | Possible Values | Mandatory |
 | ----- | ------------- | --------------- | --------- |
 | storageType | Which storage type to use | `session` or `local` | Yes |
 | storageKey | Key to store the data against | any key you like | No, can be omitted for things like `clear` which doesn't need a key |
 | storageAction | What to do with the storage | `save`, `delete`, `load`, `clear` | Yes |
 | data | The data to use with the action | any json data | No, some actions don't need data like, `delete` or `clear` |
 | responseKey | The key that will have the response in a sub (if required) | any String you want | Yes, all storage actions need a response confirming they happened - otherwise why do it at all ? |


### Markdown

```json
{
    "eventType" : "markdown_action",
    "markdown" : {
        "elementId": "where_to_render_markdown",
        "data": "your markdown"
    }
}
```

| Field | What is it? | Possible Values | Mandatory |
| ----- | ----------- | --------------- | --------- |
| elementId | The element to render the markdown against | any element's `id` | Yes |
| data | The markdown | Any markdown | Yes |


## Subs

The basic structure:

```json
{
  "responseKey": "your response key",
  "data": {},
  "error" : "your error"
}
```

| Field | What is it? | Possible Values | Mandatory |
| ----- | ----------- | --------------- | --------- |
| responseKey | The key that identifies the response |  Any unique String that can be identified in the app | Yes |
| data | Any data that should be returned as part of the response | Any Json | value | No, this can be `null` to indicate there is no data but that this succeeded |
| error | Specify what failed (if any) | Any String | No, omit if there are no errors |

### Storage

Saving notes to session storage. See Storage under Ports for more information.

On Success:

```json
{
  "responseKey": "TopNotesSavedToSessionStorage",
  "data": null,
}
```

On Failure:

```json
{
  "responseKey": "TopNotesSavedToSessionStorage",
  "data": null,
  "error": "Could not save scrib.view to session storage because: your valid reason"
}
```
