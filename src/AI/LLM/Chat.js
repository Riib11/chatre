import { config } from "dotenv"
import { Configuration, OpenAIApi } from "openai";

// config .env
config()

// config openai
let configuration = new Configuration({
  "organization": process.env.OPENAI_organization,
  "apiKey": process.env.OPENAI_apiKey,
})
let openai = new OpenAIApi(configuration)

// makes a chat request
export function _createChatCompletion(chatCompletionRequest) {
  return async (onError, onSuccess) => {
    let response = await openai.createChatCompletion(chatCompletionRequest)
    if (response === undefined) onError("chat: response was undefined")
    let data = response.data
    console.log("response.data", data)
    onSuccess(data)
    return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess(x)
  }
}
