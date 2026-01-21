"""
Anesthetic plan justification analysis
"""

import pandas as pd
import numpy as np
import re
import unicodedata
import os

# Configuration
OUTPUT_DIR = './topics'
DATA_FILE = './topics/justifications.csv'
OUTPUT_FILE = f"{OUTPUT_DIR}/regex_summary.csv"

os.makedirs(OUTPUT_DIR, exist_ok=True)


def clean_justification_text(text: str) -> str:
    """
    Minimal normalization for regex analysis.
    - Lowercase, strip accents
    - Remove commas between digits (e.g., '70,000' -> '70000')
    - Remove special characters but keep common punctuation
    - Collapse extra whitespace
    """
    if pd.isna(text) or not isinstance(text, str):
        return ""
    text = text.strip().lower()
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    text = re.sub(r'(?<=\d),(?=\d)', '', text)
    text = re.sub(r'[^a-z0-9\s\.\,\;\:\!\?\'\"\(\)\[\]\-\/\_\%]+', '', text)
    text = re.sub(r'\s+', ' ', text).strip()
    return text


def group_anesthetic_plan(plan: str) -> str:
    """Map any general/neuraxial variants into canonical labels."""
    if not isinstance(plan, str):
        return 'Other'
    p = plan.lower()
    if 'general' in p:
        return 'General'
    if 'neuraxial' in p:
        return 'Neuraxial'
    return 'Other'


def compile_regex_patterns(pattern_map: dict) -> dict:
    """Compile topic -> regex pattern with IGNORECASE|DOTALL."""
    return {topic: re.compile(pattern, flags=re.IGNORECASE | re.DOTALL)
            for topic, pattern in pattern_map.items()}


def check_regex_presence(text: str, pattern: re.Pattern) -> bool:
    """Return True if pattern is found in text."""
    if pd.isna(text) or not isinstance(text, str):
        return False
    return bool(pattern.search(text))


def analyze_patterns_for_group(group_df: pd.DataFrame, compiled_patterns: dict) -> pd.DataFrame:
    """
    Compute counts, totals, and prevalence for each topic within the group.
    Returns columns: topic, count, total_applicable, prevalence, prevalence_pct, note.
    """
    results = []

    # Globally exclude preference_ga variable from all analyses
    group_df = group_df[group_df['variable'] != 'preference_ga'].copy()
    total_docs = len(group_df)

    for topic, pattern in compiled_patterns.items():
        # Exclude rows where variable == 'delirium' for the delirium topic
        if 'delirium' in topic.lower():
            subset = group_df[group_df['variable'] != 'delirium']
            total_applicable = len(subset)
            count = subset['cleaned_justification'].apply(lambda x: check_regex_presence(x, pattern)).sum()
            note = 'Excludes variable=delirium, preference_ga'
        else:
            total_applicable = total_docs
            count = group_df['cleaned_justification'].apply(lambda x: check_regex_presence(x, pattern)).sum()
            note = 'Excludes variable=preference_ga'

        prevalence = (count / total_applicable) if total_applicable > 0 else 0.0
        results.append({
            'topic': topic,
            'count': int(count),
            'total_applicable': int(total_applicable),
            'prevalence': float(prevalence),
            'prevalence_pct': float(prevalence * 100.0),
            'note': note
        })

    return pd.DataFrame(results)


def main():
    # Define regex patterns
    regex_patterns = {
        # Airway control
        'airway_control': (
            r'\b(?:'
            r'secure\s+airway|'
            r'airway\s+control(?:led)?|'
            r'controlled\s+airway|'
            r'control(?:led)?\s+ventilation'
            r')\b'
        ),

        # Age (although we only analyze age 78, this is designed to be adaptable)
        'age': (
            r'\b(?:'
            r'elderly|geriatric|octogenar\w+|nonagenar\w+|septuagenar\w+|'
            r'age(?:d)?\s*(?:>|≥|>=|over|older\s+than)?\s*(?:6[5-9]|[7-9]\d|1[01]\d)\b|'
            r'(?:6[5-9]|[7-9]\d|1[01]\d)\s*(?:yo|y\/o|years?\s+old)'
            r')\b'
        ),

        # Sex (avoid single-letter M/F to prevent Mallampati false positives)
        'sex': r'\b(?:male|female|man|woman|sex|gender)\b',

        # Race/ethnicity (any)
        'race_ethnicity': (
            r'(?:'
            r'(?:\brace|ethnicity)\s*[:\-]?\s*'
            r'(?:non-?\s?hispanic\s+white|asian|black|white|caucasian|hispanic|latino|latina|latinx|latine|'
            r'african\s+american|middle\s+eastern|north\s+african|mena|american\s+indian|native\s+american|'
            r'alaska(?:n)?\s+native|native\s+hawaiian|pacific\s+islander|east\s+asian|south(?:east)?\s+asian)\b'
            r'|'
            r'\bnon-?\s?hispanic\s+white\b'
            r'|'
            r'\b(?:american\s+indian|native\s+american|alaska(?:n)?\s+native|native\s+hawaiian|pacific\s+islander|'
            r'african\s+american|middle\s+eastern|north\s+african|mena|hispanic|latino|latina|latinx|latine|'
            r'east\s+asian|south(?:east)?\s+asian)\b'
            r'|'
            r'\b(?:asian|black|white|caucasian)\b(?=\s+(?:patient|pt|male|female|man|woman|person|individual|adult|elderly)\b)'
            r')'
        ),

        # Social factors (any)
        'social_factors': (
            r'(?:'
            # Housing insecurity
            r'\b(?:unhoused|homeless(?:ness)?|houseless(?:ness)?|unsheltered|unstably\s+housed|housing\s+instability|'
            r'no\s+(?:fixed|permanent|stable)\s+address|no\s+stable\s+housing|lives?\s+in\s+(?:a\s+)?shelter|shelter\s+resident)\b'
            r'|'
            # Immigration/documentation status
            r'\b(?:undocumented(?:\s+immigrant)?|without\s+(?:papers|documentation)|no\s+(?:papers|documentation|legal\s+status)|'
            r'without\s+(?:immigration|legal)\s+status|not\s+lawfully\s+present|illegal\s+(?:immigrant|alien))\b'
            r'|'
            # Injection drug use
            r'\b(?:ivdu|ivda|iv\s+drug\s+use|intravenous\s+drug\s+use|injection\s+drug\s+use|injects?\s+drugs?|'
            r'people\s+who\s+inject\s+drugs|pwid)\b'
            r')'
        ),

        # Hemodynamics: stress/instability vs control/stability
        'hemodynamics': (
            r'\b(?:'
            r'hemodynamically|hemodynamic(?:s)?|'
            r'hemodynamic\s+(?:control|stabilit(?:y|ies))|'
            r'stable\s+hemodynamics|'
            r'cardio(?:vascular|pulmonary)\s+stress|'
            r'cardiac\s+stress|'
            r'hypotension|'
            r'labile\s+blood\s+pressure'
            r')\b'
        ),

        # Postoperative analgesia / pain control
        'postoperative_analgesia': (
            r'\bpost[\-\s]?op(?:erative)?\s+(?:analgesi\w*|pain(?:\s+control)?)\b'
        ),

        # Delirium / postoperative cognitive outcomes
        'delirium_prevention': (
            r'\b(?:deliri\w+|pocd|post[\-\s]?op(?:erative)?\s+cognit\w+)\b'
        ),

        # Faster recovery
        'recovery_speed': (
            r'\b(?:fast(?:er)?|rapid|early)\s+recovery\b|\benhanced\s+recovery\b'
        ),

        # Opioid use (any mention)
        'opioid_use': r'\bopioids?\b',

        # Early mobilization / ambulation
        'mobilization': (
            r'\b(?:'
            r'(?:early\s+)?mobilization|'
            r'(?:early\s+)?ambulation|'
            r'(?:early\s+)?mobiliz(?:e|es|ed|ing)|'
            r'(?:early\s+)?ambulat(?:e|es|ed|ing)'
            r')\b'
        ),

        # Thrombosis / thromboembolism risk reduction (DVT/PE/VTE)
        'thrombosis_risk': (
            r'\b(?:'
            r'venous\s+thromboembolism|vte|'
            r'deep\s+vein\s+thrombosis|dvt|'
            r'pulmonary\s+embol(?:ism|us)|'
            r'thromboemboli(?:sm|c)|'
            r'thrombosis|thrombotic|thromboembolism|thromboembolic|'
            r'blood\s+clots?'
            r')\b'
        ),

        # Bleeding risk / anticoagulation contraindication
        'bleeding_risk': (
            r'\b(?:'
            r'bleed(?:s|ing)?|'                              # bleed, bleeding
            r'hemorrhag(?:e|ic|ing)|'                        # hemorrhage, hemorrhagic
            r'(?:neuraxial\s+)?hematoma|'                    # neuraxial hematoma, hematoma
            r'coagulopath(?:y|ies)|'                         # coagulopathy
            r'thrombocytopen(?:ia|ic)|'                      # thrombocytopenia/thrombocytopenic
            r'anticoagul(?:ant|ation)|'                      # anticoagulant/anticoagulation
            r'antiplatelet(?:\s+therapy)?|'                  # antiplatelet (therapy)
            r'on\s+(?:warfarin|heparin|enoxaparin|lovenox|'
            r'apixaban|rivaroxaban|edoxaban|dabigatran|'
            r'clopidogrel|prasugrel|ticagrelor)|'            # common agents
            r'platelets?(?:\s+count)?\s*(?:=|:)?\s*(?:\d{2,3}\s*k|\d{2,3},?\s*000)|'  # e.g., platelet count 70k / 70,000
            r'inr\s*(?:>|≥|>=)\s*(?:1\.[5-9]|2(?:\.\d+)?)'   # INR thresholds commonly cited for neuraxial risk
            r')\b'
        ),
    }

    # Load data
    data = pd.read_csv(DATA_FILE)

    # Prepare fields
    data['cleaned_justification'] = data['anesthetic_plan_justification'].apply(clean_justification_text)
    data['anesthetic_group'] = data['anesthetic_plan'].apply(group_anesthetic_plan)

    # Compile patterns
    compiled_patterns = compile_regex_patterns(regex_patterns)

    # Build groups: each model × {General, Neuraxial}
    summary_rows = []
    models = [m for m in data['model_name'].dropna().unique()]
    plans = ['General', 'Neuraxial']

    for model in models:
        for plan in plans:
            group_mask = (data['model_name'] == model) & (data['anesthetic_group'] == plan)
            group_df = data[group_mask].copy()

            if group_df.empty:
                # Still emit rows with zeros to keep table rectangular
                for topic in compiled_patterns.keys():
                    summary_rows.append({
                        'model': model,
                        'plan': plan,
                        'topic': topic,
                        'count': 0,
                        'total_applicable': 0,
                        'prevalence': 0.0,
                        'prevalence_pct': 0.0,
                        'note': ''
                    })
                continue

            stats_df = analyze_patterns_for_group(group_df, compiled_patterns)
            stats_df.insert(0, 'plan', plan)
            stats_df.insert(0, 'model', model)
            summary_rows.append(stats_df)

    # Concatenate and save single tidy table
    summary_df = pd.concat(summary_rows, ignore_index=True)
    # Ordering for readability
    summary_df = summary_df[['model', 'plan', 'topic', 'count', 'total_applicable', 'prevalence', 'prevalence_pct', 'note']]
    summary_df.sort_values(by=['topic', 'model', 'plan'], inplace=True)
    summary_df.to_csv(OUTPUT_FILE, index=False)
    print(f"Saved: {OUTPUT_FILE}")
    print(f"Rows: {len(summary_df)}")


if __name__ == "__main__":
    main()