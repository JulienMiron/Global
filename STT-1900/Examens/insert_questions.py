#!/usr/bin/env python3
"""
Script to insert missing exam questions into recueil.tex.
Parses exam files, extracts individual questions, and inserts them
into the correct module sections of recueil.tex.
"""

import re
import os
import copy

BASE = os.path.dirname(os.path.abspath(__file__))
EXAM_BASE = os.path.dirname(BASE)  # STT-1900/Examens/

RECUEIL_PATH = os.path.join(BASE, 'recueil.tex')

# Exam files to process
EXAM_FILES = {
    'intra/A22': os.path.join(EXAM_BASE, 'Intra', 'A22.tex'),
    'intra/H23': os.path.join(EXAM_BASE, 'Intra', 'H23.tex'),
    'intra/H24': os.path.join(EXAM_BASE, 'Intra', 'H24.tex'),
    'intra/A24': os.path.join(EXAM_BASE, 'Intra', 'A24.tex'),
    'intra/H25': os.path.join(EXAM_BASE, 'Intra', 'H25.tex'),
    'intra/H26': os.path.join(EXAM_BASE, 'Intra', 'H26.tex'),
    'intra/H26r': os.path.join(EXAM_BASE, 'Intra', 'H26 reprise.tex'),
    'final/A22': os.path.join(EXAM_BASE, 'Final', 'A22.tex'),
    'final/H23': os.path.join(EXAM_BASE, 'Final', 'H23.tex'),
    'final/A23': os.path.join(EXAM_BASE, 'Final', 'A23.tex'),
    'final/H24': os.path.join(EXAM_BASE, 'Final', 'H24.tex'),
    'final/A24': os.path.join(EXAM_BASE, 'Final', 'A24.tex'),
    'final/A25': os.path.join(EXAM_BASE, 'Final', 'A25.tex'),
}

# Session tag mapping: exam key -> session environment name 
SESSION_TAGS = {
    'intra/A22': 'A22',
    'intra/H23': 'H23',
    'intra/H24': 'H24',
    'intra/A24': 'A24',
    'intra/H25': 'H25',
    'intra/H26': 'H26',
    'intra/H26r': 'H26r',
    'final/A22': 'A22',
    'final/H23': 'H23',
    'final/A23': 'A23',
    'final/H24': 'H24',
    'final/A24': 'A24',
    'final/A25': 'A25',
}

# Mapping: (exam_key, 0-based question index) -> module key
# Only includes questions that need to be INSERTED (missing from recueil)
QUESTION_MODULE_MAP = {
    # ============================================================
    # A22 Intra (ALL 12 questions missing - 0 A22 entries in recueil)
    # ============================================================
    ('intra/A22', 0): 'mod1',    # Q1 montage bloc parallèle
    ('intra/A22', 1): 'mod3',    # Q2 normale bivariée
    ('intra/A22', 2): 'mod7',    # Q3 IC QCM
    ('intra/A22', 3): 'mod6',    # Q4 combinaison linéaire
    ('intra/A22', 4): 'mod6',    # Q5 V/F 5 items
    ('intra/A22', 5): 'mod1',    # Q6 population lunettes/gauchères Bayes
    ('intra/A22', 6): 'mod2',    # Q7 densité triangulaire
    ('intra/A22', 7): 'mod3',    # Q8 train Neuchâtel TCL
    ('intra/A22', 8): 'lois',    # Q9 conjointe 12xy(1-x)
    ('intra/A22', 9): 'mod5',    # Q10 boulangerie boxplot
    ('intra/A22', 10): 'mod6',   # Q11 estimateurs N(12,4)
    ('intra/A22', 11): 'mod7',   # Q12 IC prix 28 transactions

    # ============================================================
    # H23 Intra (4 missing out of 11)
    # ============================================================
    ('intra/H23', 4): 'mod3',    # Q5 normale bivariée X2|X1=101
    ('intra/H23', 6): 'mod6',    # Q7 V/F 6 énoncés
    ('intra/H23', 9): 'lois',    # Q10 conjointe uniforme rectangle
    ('intra/H23', 10): 'mod5',   # Q11 microtechnique boxplot

    # ============================================================
    # H24 Intra (4 missing)
    # ============================================================
    ('intra/H24', 1): 'mod3',    # Q2 normale bivariée soudures
    ('intra/H24', 4): 'mod6',    # Q5 V/F 5 items
    ('intra/H24', 9): 'mod6',    # Q10 estimateurs X1,X2~N(0,4)
    ('intra/H24', 10): 'mod3',   # Q11 Vacherin 100 meules TCL

    # ============================================================
    # A24 Intra (2 missing)
    # ============================================================
    ('intra/A24', 3): 'mod1',    # Q4 usine machines A/B Bayes
    ('intra/A24', 8): 'mod5',    # Q9 augmenter x̄

    # ============================================================
    # H25 Intra (ALL 14 questions missing)
    # ============================================================
    ('intra/H25', 0): 'mod3',    # Q1 P(X>Y)
    ('intra/H25', 1): 'mod1',    # Q2 P(A)=0.45 cochez vrais
    ('intra/H25', 2): 'mod2',    # Q3 survie S(x)
    ('intra/H25', 3): 'mod2',    # Q4 répartition QCM
    ('intra/H25', 4): 'mod1',    # Q5 billes étoiles Bayes
    ('intra/H25', 5): 'mod6',    # Q6 deux échantillons
    ('intra/H25', 6): 'mod6',    # Q7 V/F
    ('intra/H25', 7): 'mod5',    # Q8 boxplot 21 valeurs
    ('intra/H25', 8): 'lois',    # Q9 conjointe fractionnelle
    ('intra/H25', 9): 'mod7',    # Q10 IC fréquence cardiaque
    ('intra/H25', 10): 'mod2',   # Q11 Weibull F(x)
    ('intra/H25', 11): 'mod6',   # Q12 S²_X > 2S²_Y Fisher
    ('intra/H25', 12): 'mod3',   # Q13 Gruyère TCL
    ('intra/H25', 13): 'mod7',   # Q14 calibrer machine variance

    # ============================================================
    # H26 Intra (6 missing; Q4 skipped as duplicate)
    # ============================================================
    ('intra/H26', 0): 'mod5',    # Q1 histogramme quantile 0.30
    ('intra/H26', 1): 'mod1',    # Q2 V/F 4 énoncés probabilités
    ('intra/H26', 2): 'mod7',    # Q3 IC fréquence cardiaque V/F
    ('intra/H26', 7): 'mod3',    # Q8 Lucie/Camille + TCL
    ('intra/H26', 10): 'mod6',   # Q11 X~N(0,16) n=121
    ('intra/H26', 11): 'mod7',   # Q12 IC σ²=400

    # ============================================================
    # H26 reprise (ALL 11 questions missing)
    # ============================================================
    ('intra/H26r', 0): 'mod5',   # Q1 histogramme quantile 0.60
    ('intra/H26r', 1): 'mod1',   # Q2 V/F 4 énoncés
    ('intra/H26r', 2): 'mod7',   # Q3 IC temps réaction 16 étudiants
    ('intra/H26r', 3): 'mod1',   # Q4 sac dés Bayes
    ('intra/H26r', 4): 'mod2',   # Q5 densité par morceaux
    ('intra/H26r', 5): 'mod3',   # Q6 Marc/Sophie + TCL
    ('intra/H26r', 6): 'mod4',   # Q7 covariance E(3+4X-Y+W)
    ('intra/H26r', 7): 'mod5',   # Q8 ergonomie boxplot
    ('intra/H26r', 8): 'mod6',   # Q9 estimateurs
    ('intra/H26r', 9): 'mod6',   # Q10 X~N(0,25) n=121
    ('intra/H26r', 10): 'mod7',  # Q11 IC σ²=400 n=100 90%

    # ============================================================
    # A22 Final (ALL 13 questions missing)
    # ============================================================
    ('final/A22', 0): 'mod7',     # Q1 IC V/F
    ('final/A22', 1): 'multi',    # Q2 affirmation fausse QCM
    ('final/A22', 2): 'multi',    # Q3 mises en situation → test
    ('final/A22', 3): 'mod8',     # Q4 palettes chocolat μ=5
    ('final/A22', 4): 'mod8',     # Q5 pneus/ampoules règles décision
    ('final/A22', 5): 'mod8',     # Q6 puissance claviers σ²=4
    ('final/A22', 6): 'anova2',   # Q7 4 figures ANOVA 2F
    ('final/A22', 7): 'anova2',   # Q8 agent immobilier canton+type
    ('final/A22', 8): 'mod8',     # Q9 test σ₁²=σ₂² F=0.15
    ('final/A22', 9): 'anovablocs',  # Q10 4 machines 5 opérateurs
    ('final/A22', 10): 'mod9',    # Q11 4 employés ANOVA 1F
    ('final/A22', 11): 'mod10',   # Q12 forêt amazonienne RLS
    ('final/A22', 12): 'mod11',   # Q13 régression multiple

    # ============================================================
    # H23 Final (ALL 15 questions missing)
    # ============================================================
    ('final/H23', 0): 'mod7',     # Q1 IC V/F
    ('final/H23', 1): 'mod9',     # Q2 table ANOVA fromages
    ('final/H23', 2): 'multi',    # Q3 affirmation fausse
    ('final/H23', 3): 'anovablocs',  # Q4 freins vélo (mise en situation)
    ('final/H23', 4): 'anovablocs',  # Q5 freins vélo
    ('final/H23', 5): 'anovablocs',  # Q6 freins vélo
    ('final/H23', 6): 'multi',    # Q7 V/F 5 items ANOVA/seuil
    ('final/H23', 7): 'mod8',     # Q8 Aurélien temps attente
    ('final/H23', 8): 'mod8',     # Q9 2 pop IC μ₁-μ₂
    ('final/H23', 9): 'anova2',   # Q10 agent immobilier
    ('final/H23', 10): 'anova2',  # Q11 4 sorties ANOVA 2F
    ('final/H23', 11): 'mod8',    # Q12 puissance
    ('final/H23', 12): 'mod8',    # Q13 test σ₁²=σ₂²
    ('final/H23', 13): 'mod10',   # Q14 Burtigny RLS
    ('final/H23', 14): 'mod8',    # Q15 loi exponentielle décalée

    # ============================================================
    # A23 Final (missing ones not already in recueil)
    # Already present: Q3(mod9), Q5(mod8), Q9(mod8), Q10(mod8), Q13(mod8), Q16(mod10)
    # ============================================================
    ('final/A23', 0): 'mod7',     # Q1 IC V/F
    ('final/A23', 1): 'multi',    # Q2 affirmation fausse
    ('final/A23', 3): 'mod7',     # Q4 IC 10 formules
    ('final/A23', 5): 'anovablocs',  # Q6 freins vélo
    ('final/A23', 6): 'anovablocs',  # Q7 freins vélo
    ('final/A23', 7): 'anovablocs',  # Q8 freins vélo
    ('final/A23', 10): 'anova2',  # Q11 4 sorties ANOVA 2F
    ('final/A23', 11): 'anova2',  # Q12 chef fondue
    ('final/A23', 13): 'anovablocs',  # Q14 4 machines
    ('final/A23', 14): 'mod9',    # Q15 4 employés ANOVA 1F

    # ============================================================
    # H24 Final (3 missing)
    # ============================================================
    ('final/H24', 4): 'multi',    # Q5 3 scénarios ANOVA (mise en situation)
    ('final/H24', 5): 'multi',    # Q6 associer scénario → table
    ('final/H24', 6): 'multi',    # Q7 conclusion ANOVA

    # ============================================================
    # A24 Final (7 missing)
    # ============================================================
    ('final/A24', 0): 'mod7',     # Q1 IC V/F
    ('final/A24', 1): 'anovablocs',  # Q2 PPDS blocs
    ('final/A24', 2): 'anova2',   # Q3 sortie ANOVA 2F
    ('final/A24', 4): 'mod8',     # Q5 20 statistiques observées
    ('final/A24', 9): 'anovablocs',  # Q10 4 machines
    ('final/A24', 14): 'anova2',  # Q15 analyste politique
    ('final/A24', 15): 'mod10',   # Q16 postulats RLS 3 figures

    # ============================================================
    # A25 Final (1 missing)
    # ============================================================
    ('final/A25', 13): 'mod10',   # Q14 Or/minerai Y~N(10+10x,100)
}

# Module insertion order (used to find insertion points)
MODULE_ORDER = [
    'mod1', 'mod2', 'mod3', 'mod4', 'mod5', 'mod6', 'mod7',
    'mod8', 'mod9', 'mod10', 'mod11', 'multi',
    'lois', 'anova2', 'anovablocs'
]


def extract_questions(filepath):
    """Extract individual questions from an exam file.
    Returns a list of question texts (0-indexed).
    """
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find the region between \begin{questions} and \end{questions}
    # (only the FIRST such pair)
    begin_match = re.search(r'\\begin\{questions\}', content)
    end_match = re.search(r'\\end\{questions\}', content)
    
    if not begin_match or not end_match:
        print(f"WARNING: Could not find questions region in {filepath}")
        return []
    
    questions_region = content[begin_match.end():end_match.start()]
    
    # Split at \question markers
    # Pattern: \question followed by optional points and/or label
    parts = re.split(r'(?=\\question(?:\s*\[.*?\])?\s*(?:\\label\{.*?\})?)', questions_region)
    
    questions = []
    for part in parts:
        part = part.strip()
        if part and part.startswith('\\question'):
            questions.append(part)
    
    return questions


def clean_question_for_recueil(text, session_tag):
    """Clean a question for insertion into the recueil.
    - Remove point values from \\question[N]
    - Remove \\newpage, \\pageblanche, \\pagesuivante{}, \\suite{}
    - Remove \\emptybox{}, \\boite{}{} 
    - Remove \\vfill and standalone \\mbox{}
    - Add session tag after \\question
    """
    # Remove \label{...} from \question line 
    text = re.sub(r'\\label\{[^}]*\}', '', text, count=1)
    
    # Replace \question[N] or \question with \question\n(TAG)
    text = re.sub(
        r'^\\question(?:\s*\[.*?\])?',
        f'\\\\question\n({session_tag})',
        text,
        count=1
    )
    
    # Remove exam layout commands
    text = re.sub(r'\\newpage\s*', '', text)
    text = re.sub(r'\\pageblanche\s*', '', text)
    text = re.sub(r'\\pagesuivante\{[^}]*\}\s*', '', text)
    text = re.sub(r'\\suite\{[^}]*\}\s*', '', text)
    text = re.sub(r'\\emptybox\{[^}]*\}\s*', '', text)
    text = re.sub(r'\\boite\{[^}]*\}\{[^}]*\}\s*', '', text)
    text = re.sub(r'\\boxitem\{', '{', text)  # Replace \boxitem{ with just {
    text = re.sub(r'\\vfill\s*', '', text)
    text = re.sub(r'^\s*\\mbox\{\}\s*$', '', text, flags=re.MULTILINE)
    
    # Remove excessive blank lines (more than 2 → 2)
    text = re.sub(r'\n{3,}', '\n\n', text)
    
    # Remove trailing whitespace
    text = text.rstrip()
    
    return text


def find_module_line_ranges(recueil_lines):
    """Find the line ranges for each module in the recueil.
    Returns dict: module_key -> (start_line_idx, end_line_idx)
    where end_line_idx is the line BEFORE the next module header.
    """
    module_starts = {}
    
    for i, line in enumerate(recueil_lines):
        # Match \moduleheader{key}{title} or \modulegroupheader{key}{title}
        m = re.match(r'\\moduleheader\{(\w+)\}', line)
        if m:
            module_starts[m.group(1)] = i
        m = re.match(r'\\modulegroupheader\{(\w+)\}', line)
        if m:
            # Group headers don't define a module section themselves
            pass
    
    # Determine end lines
    module_ranges = {}
    sorted_modules = sorted(module_starts.items(), key=lambda x: x[1])
    
    for idx, (key, start) in enumerate(sorted_modules):
        if idx + 1 < len(sorted_modules):
            end = sorted_modules[idx + 1][1]
        else:
            # Last module: find \end{questions}
            end = len(recueil_lines)
            for i in range(len(recueil_lines) - 1, start, -1):
                if '\\end{questions}' in recueil_lines[i]:
                    end = i
                    break
        module_ranges[key] = (start, end)
    
    return module_ranges


def main():
    # Step 1: Read recueil
    with open(RECUEIL_PATH, 'r', encoding='utf-8') as f:
        recueil_content = f.read()
    recueil_lines = recueil_content.split('\n')
    
    # Step 2: Find module ranges
    module_ranges = find_module_line_ranges(recueil_lines)
    print("Module ranges:")
    for key, (start, end) in sorted(module_ranges.items(), key=lambda x: x[1][0]):
        print(f"  {key}: lines {start+1}-{end}")
    
    # Step 3: Extract questions from all exam files
    all_questions = {}
    for exam_key, filepath in EXAM_FILES.items():
        if not os.path.exists(filepath):
            print(f"WARNING: File not found: {filepath}")
            continue
        questions = extract_questions(filepath)
        all_questions[exam_key] = questions
        print(f"Extracted {len(questions)} questions from {exam_key}")
    
    # Step 4: Collect questions to insert per module
    insertions = {mod: [] for mod in MODULE_ORDER}
    
    for (exam_key, q_idx), module_key in QUESTION_MODULE_MAP.items():
        if exam_key not in all_questions:
            print(f"WARNING: No questions extracted for {exam_key}")
            continue
        if q_idx >= len(all_questions[exam_key]):
            print(f"WARNING: Q{q_idx+1} index out of range for {exam_key} (has {len(all_questions[exam_key])} questions)")
            continue
        
        session_tag = SESSION_TAGS[exam_key]
        raw_text = all_questions[exam_key][q_idx]
        cleaned = clean_question_for_recueil(raw_text, session_tag)
        
        insertions[module_key].append({
            'text': cleaned,
            'session': session_tag,
            'exam_key': exam_key,
            'q_idx': q_idx,
        })
    
    # Step 5: Build insertion blocks per module
    insertion_blocks = {}
    for module_key in MODULE_ORDER:
        if not insertions[module_key]:
            continue
        
        lines = []
        for item in insertions[module_key]:
            lines.append(f"\n\\begin{{{item['session']}}}")
            lines.append(item['text'])
            lines.append(f"\\end{{{item['session']}}}")
        
        insertion_blocks[module_key] = '\n'.join(lines)
    
    # Step 6: Insert into recueil (working from bottom to top to preserve line numbers)
    # For each module, insert just before the next module header
    sorted_modules = sorted(module_ranges.items(), key=lambda x: x[1][0], reverse=True)
    
    new_lines = list(recueil_lines)
    
    for module_key, (start, end) in sorted_modules:
        if module_key not in insertion_blocks:
            continue
        
        block = insertion_blocks[module_key]
        count = len(insertions[module_key])
        
        # Insert just before the end of this module section
        # Find the right insertion point: just before the next moduleheader or modulegroupheader
        insert_at = end
        
        # Check if there's a \modulegroupheader just before the next \moduleheader
        # Handle the special case for the 'hors' group
        if module_key == 'multi':
            # Insert before \modulegroupheader{hors}
            for i in range(end - 1, start, -1):
                if '\\modulegroupheader' in new_lines[i]:
                    insert_at = i
                    break
        
        print(f"Inserting {count} questions into {module_key} at line {insert_at + 1}")
        
        # Split block into lines and insert
        block_lines = block.split('\n')
        for i, bl in enumerate(block_lines):
            new_lines.insert(insert_at + i, bl)
    
    # Step 7: Write output
    output_path = RECUEIL_PATH  # Overwrite
    backup_path = RECUEIL_PATH + '.bak'
    
    # Create backup
    with open(backup_path, 'w', encoding='utf-8') as f:
        f.write(recueil_content)
    print(f"\nBackup saved to {backup_path}")
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(new_lines))
    
    total_inserted = sum(len(v) for v in insertions.values())
    print(f"\nDone! Inserted {total_inserted} questions into {output_path}")
    print(f"New file has {len(new_lines)} lines (was {len(recueil_lines)})")


if __name__ == '__main__':
    main()
