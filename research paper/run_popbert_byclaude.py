#DISCLAIMER: THIS IS CODE WRITTEN BY CLAUDE

import pandas as pd
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

# Load model and tokenizer
tokenizer = AutoTokenizer.from_pretrained("luerhard/PopBERT")
model = AutoModelForSequenceClassification.from_pretrained("luerhard/PopBERT")
model.eval()

# Load data
bt_speeches = pd.read_csv("data/bundestag_seit2020.csv", index_col="Unnamed: 0")

bt_speeches = bt_speeches.sample(frac=0.01)
texts = list(bt_speeches["Speech"])


def predict_long_text(text, tokenizer, model, max_length=512, stride=256):
    # Tokenize without truncation to get all tokens
    tokens = tokenizer(text, return_tensors="pt", truncation=False)
    input_ids = tokens["input_ids"][0]  # shape: (num_tokens,)

    all_probs = []

    for start in range(0, len(input_ids), stride):
        chunk_ids = input_ids[start : start + max_length]

        # Skip very short trailing chunks
        if len(chunk_ids) < 10:
            break

        chunk_ids = chunk_ids.unsqueeze(0)  # add batch dimension
        attention_mask = torch.ones_like(chunk_ids)

        with torch.inference_mode():
            out = model(input_ids=chunk_ids, attention_mask=attention_mask)

        probs = torch.sigmoid(out.logits).detach()
        all_probs.append(probs)

    # Average probabilities across all chunks
    return torch.stack(all_probs).mean(dim=0).numpy()[0]


# Run prediction on all speeches
all_probs = []
for i, text in enumerate(texts):
    print(f"Processing speech {i+1}/{len(texts)}...")
    probs = predict_long_text(text, tokenizer, model)
    all_probs.append(probs)

# Build results dataframe
label_names = model.config.id2label  # e.g. {0: 'elite', 1: 'people', ...}
prob_df = pd.DataFrame(
    all_probs,
    columns=[label_names[i] for i in range(len(label_names))]
)
results = pd.concat([bt_speeches.reset_index(drop=True), prob_df], axis=1)

print(results.head())
results.to_csv("data/bundestag_seit2020_predicted.csv", index=False)


