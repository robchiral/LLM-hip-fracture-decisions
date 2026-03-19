"""
Build justification tables and regex-prevalence summaries for the main and
commercial model outputs.
"""

from pathlib import Path
import re
import unicodedata

import pandas as pd


ANALYSIS_DIR = Path("/Users/robertchen/Documents/GitHub/LLM_hip/analysis")
DATA_DIR = ANALYSIS_DIR / "data"
TOPICS_DIR = ANALYSIS_DIR / "topics"

MAIN_SOURCE_FILES = [
    DATA_DIR / "openai.tsv",
    DATA_DIR / "gemini.tsv",
    DATA_DIR / "deepseek.tsv",
]
COMMERCIAL_SOURCE_FILE = DATA_DIR / "commercial.tsv"

MAIN_JUSTIFICATIONS_OUTPUT = TOPICS_DIR / "justifications.csv"
MAIN_SUMMARY_OUTPUT = TOPICS_DIR / "regex_summary.csv"
MAIN_CONTROL_SUMMARY_OUTPUT = TOPICS_DIR / "control_regex_summary.csv"
COMMERCIAL_JUSTIFICATIONS_OUTPUT = TOPICS_DIR / "commercial_justifications.tsv"
COMMERCIAL_SUMMARY_OUTPUT = TOPICS_DIR / "commercial_regex_summary.tsv"

JUSTIFICATION_COLUMNS = [
    "model_name",
    "surgery_id",
    "sex",
    "variable",
    "replicate_id",
    "anesthetic_plan",
    "anesthetic_plan_justification",
]
REQUIRED_COLUMNS = [
    "model_name",
    "surgery_id",
    "sex",
    "replicate_id",
    "anesthetic_plan",
    "anesthetic_plan_justification",
]
PLAN_LEVELS = ["General", "Neuraxial"]


REGEX_PATTERNS = {
    "airway_control": (
        r"\b(?:"
        r"secure\s+airway|"
        r"airway\s+control(?:led)?|"
        r"controlled\s+airway|"
        r"control(?:led)?\s+ventilation"
        r")\b"
    ),
    "age": (
        r"\b(?:"
        r"elderly|geriatric|octogenar\w+|nonagenar\w+|septuagenar\w+|"
        r"age(?:d)?\s*(?:>|≥|>=|over|older\s+than)?\s*(?:6[5-9]|[7-9]\d|1[01]\d)\b|"
        r"(?:6[5-9]|[7-9]\d|1[01]\d)\s*(?:yo|y\/o|years?\s+old)"
        r")\b"
    ),
    "sex": r"\b(?:male|female|man|woman|sex|gender)\b",
    "race_ethnicity": (
        r"(?:"
        r"(?:\brace|ethnicity)\s*[:\-]?\s*"
        r"(?:non-?\s?hispanic\s+white|asian|black|white|caucasian|hispanic|latino|latina|latinx|latine|"
        r"african\s+american|middle\s+eastern|north\s+african|mena|american\s+indian|native\s+american|"
        r"alaska(?:n)?\s+native|native\s+hawaiian|pacific\s+islander|east\s+asian|south(?:east)?\s+asian)\b"
        r"|"
        r"\bnon-?\s?hispanic\s+white\b"
        r"|"
        r"\b(?:american\s+indian|native\s+american|alaska(?:n)?\s+native|native\s+hawaiian|pacific\s+islander|"
        r"african\s+american|middle\s+eastern|north\s+african|mena|hispanic|latino|latina|latinx|latine|"
        r"east\s+asian|south(?:east)?\s+asian)\b"
        r"|"
        r"\b(?:asian|black|white|caucasian)\b(?=\s+(?:patient|pt|male|female|man|woman|person|individual|adult|elderly)\b)"
        r")"
    ),
    "social_factors": (
        r"(?:"
        r"\b(?:unhoused|homeless(?:ness)?|houseless(?:ness)?|unsheltered|unstably\s+housed|housing\s+instability|"
        r"no\s+(?:fixed|permanent|stable)\s+address|no\s+stable\s+housing|lives?\s+in\s+(?:a\s+)?shelter|shelter\s+resident)\b"
        r"|"
        r"\b(?:undocumented(?:\s+immigrant)?|without\s+(?:papers|documentation)|no\s+(?:papers|documentation|legal\s+status)|"
        r"without\s+(?:immigration|legal)\s+status|not\s+lawfully\s+present|illegal\s+(?:immigrant|alien))\b"
        r"|"
        r"\b(?:ivdu|ivda|iv\s+drug\s+use|intravenous\s+drug\s+use|injection\s+drug\s+use|injects?\s+drugs?|"
        r"people\s+who\s+inject\s+drugs|pwid)\b"
        r")"
    ),
    "hemodynamics": (
        r"\b(?:"
        r"hemodynamically|hemodynamic(?:s)?|"
        r"hemodynamic\s+(?:control|stabilit(?:y|ies))|"
        r"stable\s+hemodynamics|"
        r"cardio(?:vascular|pulmonary)\s+stress|"
        r"cardiac\s+stress|"
        r"hypotension|"
        r"labile\s+blood\s+pressure"
        r")\b"
    ),
    "postoperative_analgesia": (
        r"\bpost[\-\s]?op(?:erative)?\s+(?:analgesi\w*|pain(?:\s+control)?)\b"
    ),
    "delirium_prevention": (
        r"\b(?:deliri\w+|pocd|post[\-\s]?op(?:erative)?\s+cognit\w+)\b"
    ),
    "recovery_speed": (
        r"\b(?:fast(?:er)?|rapid|early)\s+recovery\b|\benhanced\s+recovery\b"
    ),
    "mortality": (
        r"\b(?:mortality|mortalities|death|deaths|survival|survive|survives|survived|"
        r"surviv(?:al|e|ed|es))\b"
    ),
    "opioid_use": r"\bopioids?\b",
    "mobilization": (
        r"\b(?:"
        r"(?:early\s+)?mobilization|"
        r"(?:early\s+)?ambulation|"
        r"(?:early\s+)?mobiliz(?:e|es|ed|ing)|"
        r"(?:early\s+)?ambulat(?:e|es|ed|ing)"
        r")\b"
    ),
    "thrombosis_risk": (
        r"\b(?:"
        r"venous\s+thromboembolism|vte|"
        r"deep\s+vein\s+thrombosis|dvt|"
        r"pulmonary\s+embol(?:ism|us)|"
        r"thromboemboli(?:sm|c)|"
        r"thrombosis|thrombotic|thromboembolism|thromboembolic|"
        r"blood\s+clots?"
        r")\b"
    ),
    "bleeding_risk": (
        r"\b(?:"
        r"bleed(?:s|ing)?|"
        r"hemorrhag(?:e|ic|ing)|"
        r"(?:neuraxial\s+)?hematoma|"
        r"coagulopath(?:y|ies)|"
        r"thrombocytopen(?:ia|ic)|"
        r"anticoagul(?:ant|ation)|"
        r"antiplatelet(?:\s+therapy)?|"
        r"on\s+(?:warfarin|heparin|enoxaparin|lovenox|"
        r"apixaban|rivaroxaban|edoxaban|dabigatran|"
        r"clopidogrel|prasugrel|ticagrelor)|"
        r"platelets?(?:\s+count)?\s*(?:=|:)?\s*(?:\d{2,3}\s*k|\d{2,3},?\s*000)|"
        r"inr\s*(?:>|≥|>=)\s*(?:1\.[5-9]|2(?:\.\d+)?)"
        r")\b"
    ),
}


def infer_sep(path: Path) -> str:
    return "\t" if path.suffix.lower() == ".tsv" else ","


def clean_justification_text(text: str) -> str:
    if pd.isna(text) or not isinstance(text, str):
        return ""
    text = text.strip().lower()
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    text = re.sub(r"(?<=\d),(?=\d)", "", text)
    text = re.sub(r"[^a-z0-9\s\.\,\;\:\!\?\'\"\(\)\[\]\-\/\_\%]+", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def group_anesthetic_plan(plan: str) -> str:
    if not isinstance(plan, str):
        return "Other"
    lowered = plan.lower()
    if "general" in lowered:
        return "General"
    if "neuraxial" in lowered:
        return "Neuraxial"
    return "Other"


def compile_regex_patterns(pattern_map: dict) -> dict:
    return {
        topic: re.compile(pattern, flags=re.IGNORECASE | re.DOTALL)
        for topic, pattern in pattern_map.items()
    }


def check_regex_presence(text: str, pattern: re.Pattern) -> bool:
    if pd.isna(text) or not isinstance(text, str):
        return False
    return bool(pattern.search(text))


def analyze_patterns_for_group(group_df: pd.DataFrame, compiled_patterns: dict) -> pd.DataFrame:
    results = []
    group_df = group_df[group_df["variable"] != "preference_ga"].copy()
    total_docs = len(group_df)

    for topic, pattern in compiled_patterns.items():
        if "delirium" in topic.lower():
            subset = group_df[group_df["variable"] != "delirium"]
            total_applicable = len(subset)
            count = subset["cleaned_justification"].apply(
                lambda x: check_regex_presence(x, pattern)
            ).sum()
            note = "Excludes variable=delirium, preference_ga"
        else:
            total_applicable = total_docs
            count = group_df["cleaned_justification"].apply(
                lambda x: check_regex_presence(x, pattern)
            ).sum()
            note = "Excludes variable=preference_ga"

        prevalence = (count / total_applicable) if total_applicable > 0 else 0.0
        results.append(
            {
                "topic": topic,
                "count": int(count),
                "total_applicable": int(total_applicable),
                "prevalence": float(prevalence),
                "prevalence_pct": float(prevalence * 100.0),
                "note": note,
            }
        )

    return pd.DataFrame(results)


def load_table(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(f"Missing input file: {path}")
    return pd.read_csv(path, sep=infer_sep(path))


def extract_justifications(data: pd.DataFrame) -> pd.DataFrame:
    missing_columns = [column for column in REQUIRED_COLUMNS if column not in data.columns]
    if missing_columns:
        raise ValueError(f"Input data is missing required columns: {missing_columns}")

    extracted = data.copy()
    if "variable" not in extracted.columns:
        extracted["variable"] = "control"

    return extracted[JUSTIFICATION_COLUMNS].copy()


def build_main_justifications() -> pd.DataFrame:
    frames = [extract_justifications(load_table(path)) for path in MAIN_SOURCE_FILES]
    return pd.concat(frames, ignore_index=True)


def build_commercial_justifications() -> pd.DataFrame:
    return extract_justifications(load_table(COMMERCIAL_SOURCE_FILE))


def summarize_topics(justification_table: pd.DataFrame) -> pd.DataFrame:
    data = justification_table.copy()
    data["cleaned_justification"] = data["anesthetic_plan_justification"].apply(clean_justification_text)
    data["anesthetic_group"] = data["anesthetic_plan"].apply(group_anesthetic_plan)
    compiled_patterns = compile_regex_patterns(REGEX_PATTERNS)

    summary_rows = []
    models = [model for model in data["model_name"].dropna().unique()]

    for model in models:
        for plan in PLAN_LEVELS:
            group_df = data[
                (data["model_name"] == model) & (data["anesthetic_group"] == plan)
            ].copy()

            if group_df.empty:
                empty_rows = pd.DataFrame(
                    [
                        {
                            "model": model,
                            "plan": plan,
                            "topic": topic,
                            "count": 0,
                            "total_applicable": 0,
                            "prevalence": 0.0,
                            "prevalence_pct": 0.0,
                            "note": "",
                        }
                        for topic in compiled_patterns
                    ]
                )
                summary_rows.append(empty_rows)
                continue

            stats_df = analyze_patterns_for_group(group_df, compiled_patterns)
            stats_df.insert(0, "plan", plan)
            stats_df.insert(0, "model", model)
            summary_rows.append(stats_df)

    summary_df = pd.concat(summary_rows, ignore_index=True)
    summary_df = summary_df[
        [
            "model",
            "plan",
            "topic",
            "count",
            "total_applicable",
            "prevalence",
            "prevalence_pct",
            "note",
        ]
    ].sort_values(by=["topic", "model", "plan"])
    return summary_df


def write_table(data: pd.DataFrame, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    data.to_csv(output_path, sep=infer_sep(output_path), index=False)


def run_dataset(label: str, justification_table: pd.DataFrame, justifications_output: Path, summary_output: Path) -> None:
    summary_df = summarize_topics(justification_table)
    write_table(justification_table, justifications_output)
    write_table(summary_df, summary_output)
    print(f"Saved {label} justifications to: {justifications_output}")
    print(f"Saved {label} summary to: {summary_output}")
    print(f"{label.title()} summary rows: {len(summary_df)}")


def run_summary_only(label: str, justification_table: pd.DataFrame, summary_output: Path) -> None:
    summary_df = summarize_topics(justification_table)
    write_table(summary_df, summary_output)
    print(f"Saved {label} summary to: {summary_output}")
    print(f"{label.title()} summary rows: {len(summary_df)}")


def main() -> None:
    TOPICS_DIR.mkdir(parents=True, exist_ok=True)

    main_justifications = build_main_justifications()
    main_control_justifications = main_justifications[
        main_justifications["variable"] == "control"
    ].copy()
    commercial_justifications = build_commercial_justifications()

    run_dataset(
        "main",
        main_justifications,
        MAIN_JUSTIFICATIONS_OUTPUT,
        MAIN_SUMMARY_OUTPUT,
    )
    run_summary_only(
        "main control-only",
        main_control_justifications,
        MAIN_CONTROL_SUMMARY_OUTPUT,
    )
    run_dataset(
        "commercial",
        commercial_justifications,
        COMMERCIAL_JUSTIFICATIONS_OUTPUT,
        COMMERCIAL_SUMMARY_OUTPUT,
    )


if __name__ == "__main__":
    main()
