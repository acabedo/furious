Developed with the aid of Chat-GPT (4.0)

# Introduction

Welcome to **Oralstats Furious**, a Shiny application designed to help you analyze prosodic features and speech data.
This guide will walk you through the functionalities available in the two main sections of the app: **Create** and **Furious**.

------------------------------------------------------------------------

# 1. The "Create" Section

The **Create** section allows you to upload your data files, configure settings, and run the Oralstats analysis process.
This section is divided into a sidebar for options and a main panel where results are displayed.

## 1.1 Sidebar Options

-   **Language Selection**: Choose the language of your transcription from Spanish, Catalan, English, French, or Italian.
-   **Transcription Type**: Select whether your transcription includes only phonemes or both words and phonemes (specific to Spanish).
-   **Process Button**: After uploading your files, click this button to initiate the Oralstats analysis process.
-   **Tutorial Link**: Click the link to access a tutorial for more detailed help.

## 1.2 Uploading Files

Within the "Upload" tab, you can upload the following files:

-   **Transcription Files**: Upload one or more `.txt` files with your transcription data.
-   **Pitch and Intensity Files**: These are additional `.txt` files containing pitch and intensity information for the transcription.
-   **Sentiment File (Optional)**: Upload a `.csv` file if you wish to include sentiment analysis in your prosody analysis.
-   **Audio Files**: Upload one or more audio files in `.mp3` format to synchronize the prosodic analysis with the audio.

## 1.3 Viewing and Downloading Data

Once your files are processed, you can explore the following tabs for detailed data insights:

-   **Prosody Data**: View a table with prosodic information like pitch, intensity, and other speech features.
-   **Words Data**: Examine a table of word-level data extracted from your transcription.
-   **Ips Data**: Explore intonation phrases (IPs) and their associated prosodic characteristics.
-   **Turns Data**: Analyze conversational turns, including prosodic and interactional features.
-   **Download Options**: Download CSV files for the Turns, Ips, and Words data by clicking the respective buttons.

## 1.4 Variables generated (only IPS)

### 1. **pitch_Hz**

-   **Definition**: The **pitch** of a sound refers to the perceived frequency of a speaker's voice.
    It is measured in **Hertz (Hz)**.

-   **Explanation**: Pitch is a key prosodic feature used to convey emotion, emphasis, and other linguistic cues.
    High pitch can indicate excitement or stress, while low pitch can suggest calmness or seriousness.
    In speech analysis, tracking pitch variation helps in identifying tone, intonation patterns, and emotional state.

### 2. **velocidad**

-   **Definition**: **Speed** or **velocity** of speech.

-   **Explanation**: This variable represents the rate at which words or phonemes are spoken.
    It can be measured in terms of syllables or words per second.
    Faster speech may indicate excitement, urgency, or nervousness, while slower speech often correlates with careful articulation or a relaxed state.
    This variable is critical for analyzing speech fluency and rhythm.

### 3. **range_st**

-   **Definition**: **Pitch range** measured in **semitones (st)**.

-   **Explanation**: This measures the difference between the highest and lowest pitch in a given speech segment.
    A wider pitch range can suggest dynamic or expressive speech, whereas a narrower range might indicate monotony or calmness.
    In semitone units, the range is a musically meaningful way to quantify vocal pitch variation.

### 4. **phon_inflexion_st**

-   **Definition**: **Pitch inflection** in semitones (st).

-   **Explanation**: This variable tracks how much the pitch changes within a single phoneme or speech unit.
    Inflexions are the subtle rises and falls in pitch that occur naturally during speech.
    Significant inflections can indicate questioning intonation or emotional emphasis.

### 5. **phon_intensity**

-   **Definition**: **Intensity** of speech, often perceived as loudness, measured in decibels (dB).

-   **Explanation**: Intensity refers to the amplitude of sound waves, with higher values corresponding to louder speech.
    Loudness can emphasize certain words or emotions (e.g., shouting or stress), while quieter speech might suggest calmness or insecurity.
    Measuring intensity helps analyze the speaker’s expressiveness and control over speech.

### 6. **phon_dur**

-   **Definition**: **Duration** of a phoneme or speech unit, measured in seconds.

-   **Explanation**: This represents how long individual sounds or phonemes last.
    Longer durations might indicate hesitation, emphasis, or careful articulation, whereas shorter durations are typical of fluent or rapid speech.
    Duration is essential for timing analysis in speech.

### 7. **phon_pausa_intra**

-   **Definition**: **Intra-phonemic pause** duration.

-   **Explanation**: This refers to brief pauses that occur within a single phoneme or speech sound.
    These are unusual and can be due to stuttering or hesitations, often providing insights into speech disfluency or difficulty in articulation.

### 8. **phon_pausa_inter**

-   **Definition**: **Inter-phonemic pause** duration.

-   **Explanation**: This is the duration of pauses between phonemes or words.
    Natural pauses often occur between words or phrases and can indicate the speaker's pace and fluency.
    Longer pauses might signal hesitation or cognitive load, while shorter pauses reflect fast, uninterrupted speech.

### 9. **morph_qnoun**

-   **Definition**: **Quantification of nouns**.

-   **Explanation**: This variable counts or identifies the frequency of nouns in the speech.
    A high frequency of nouns often correlates with a descriptive or factual speech style, while lower usage might indicate a more action-oriented or abstract speech.

### 10. **morph_qverb**

-   **Definition**: **Quantification of verbs**.

-   **Explanation**: This counts the occurrences of verbs in the speech.
    Verbs indicate actions or processes, and their frequency can signal an action-oriented discourse.
    Tracking verb usage helps identify whether the speaker focuses on activities or states of being.

### 11. **morph_qadv**

-   **Definition**: **Quantification of adverbs**.

-   **Explanation**: This counts how often adverbs are used in speech.
    Adverbs modify verbs, adjectives, or other adverbs, adding information about how, when, or to what extent actions occur.
    High usage might indicate detailed descriptions of actions or intensities.

### 12. **morph_qadj**

-   **Definition**: **Quantification of adjectives**.

-   **Explanation**: This variable tracks the use of adjectives, which describe or modify nouns.
    High frequency suggests a focus on descriptive language, useful in evaluating how detailed or expressive the speech is.

### 13. **morph_qdet**

-   **Definition**: **Quantification of determiners**.

-   **Explanation**: Determiners are words like "the," "a," or "this" that specify nouns.
    Their usage patterns can reflect formality, specificity, and syntactic structure in speech.

### 14. **morph_qpron**

-   **Definition**: **Quantification of pronouns**.

-   **Explanation**: This counts the occurrences of pronouns (e.g., "he," "she," "they").
    High pronoun usage is common in informal or narrative speech, while lower usage may suggest more formal, noun-heavy language.

### 15. **morph_qaux**

-   **Definition**: **Quantification of auxiliary verbs**.

-   **Explanation**: This tracks auxiliary verbs (e.g., "is," "have," "do"), which often assist in forming different tenses, moods, or voices.
    Their usage reflects the complexity of verb structures in speech.

### 16. **morph_qneg**

-   **Definition**: **Quantification of negations**.

-   **Explanation**: This tracks the frequency of negations (e.g., "not," "never").
    Negations are useful for sentiment analysis, as they can affect the polarity of statements, shifting their meaning towards the negative.

## 1.5 Variables generated (only for Spanish with words and phones)

### Key Variables Explained

1.  **Words Data**:

    -   **`vowels_structure`**: Extracts vowels from each word to help determine stress and accent.

    -   **`accent`**: Classifies the word's accent type (oxitone, paroxitone, proparoxitone, or non-tonic).

    -   **`pause`**: Measures the pause between words.

    -   **`changeip`**: Identifies changes in intonation phrases (IP) based on pauses and speaker changes.

    -   **`changeturn`**: Detects speaker turn changes.

    -   **`id_ip`**: Assigns unique IDs to intonation phrases.

    -   **`token_id`**: Identifies the position of each word within an IP.

2.  **MAS (Melodic Analysis of Speech) Structure**:

    -   **`toneme_word`**: Identifies the tonic syllable within a word (important for stress-based analysis).

    -   **`anacrusis`**: Determines if the word or phoneme appears before the tonic syllable.

    -   **`body`**: Refers to the "body" of the melodic contour.

    -   **`toneme`**: Represents the main melodic target for the tonic syllable.

3.  **Prosodic Analysis for Vowels**:

    -   **`q1piHz` to `q4piHz`**: Pitch values at different quarters of the vowel's duration.

    -   **`inflexion_percent_Hz_from_prev`**: Percentage change in pitch from the previous vowel.

    -   **`inflexion_ST_from_prev`**: Inflexion in semitones from the previous vowel.

    -   **`center_Hz`**: Average pitch in Hertz for the vowel.

    -   **`intensity`**: Average intensity in decibels for the vowel.

4.  **TOBI (Tones and Break Indices)**:

    -   **`TOBI_pattern`**: Pattern of the tonal contour (e.g., H*L, L*H) based on pitch inflections between syllables or phonemes.

5.  **IP-level Variables**:

    -   **`pitch_reset_from_ST`**: Measures pitch reset in semitones from the previous phrase.

    -   **`velocidad`**: The speed of speech, calculated as duration divided by the number of words.

    -   **`range_st`**: Pitch range in semitones.

# 2. The "Furious" Section

The **Furious** section provides more advanced analysis tools, allowing you to visualize and explore speech data in greater detail.

## 2.1 Sidebar Options

-   **Conversation Selection**: Choose a conversation file from the dropdown menu to begin your analysis.
-   **Variable Selection**: Select a specific prosodic or morphological variable (e.g., pitch, intensity) for visualization.
-   **Generate Furious Plot**: Click the button to generate visualizations for the selected conversation and variable.

## 2.2 Visualizations and Data Exploration

The **Furious** section contains various tabs to explore data through different visualizations and formats:

-   **By Variable**: View dynamic plots for the selected prosodic or morphological variable using an interactive plotly graph.
-   **Thermal View**: Create a heatmap based on multiple variables, allowing you to compare prosodic features visually.
-   **Context Tab**: Adjust the time context (in seconds) for visualizations, helping you focus on specific segments of the conversation.
-   **Detailed Data**: Examine additional data tables for more in-depth analysis.
-   **Boxplots**: View boxplots to compare distributions of selected variables across different speakers or segments.
-   **Wordclouds**: Generate word clouds for both "furious" (emotionally charged) and "non-furious" segments of speech, excluding specific parts of speech (e.g., determiners, conjunctions).
-   **Heatmap by Speaker**: Generate a heatmap that shows how different speakers use prosodic features over time.
-   **Webchart**: Visualize data using radar (web) charts, useful for comparing multiple variables simultaneously.
-   **PCA (Principal Component Analysis)**: Explore PCA visualizations to understand the key components driving variability in the data.
-   **Tree Plot**: View a decision tree generated from the data, offering insights into speech patterns or prosodic changes.

------------------------------------------------------------------------

# Conclusion

This guide provides an overview of how to use the **Oralstats Furious** Shiny application for prosody and speech analysis.
Explore the different sections to gain insights into your speech data, and don’t hesitate to refer back to this guide as needed.
