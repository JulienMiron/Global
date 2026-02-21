% Classe personnalisée BeamerJS
% Auteur : Jérôme Soucy (jsoucy@mat.ulaval.ca)
% Modification: Julien Miron (julien.miron@mat.ulaval.ca
% Créée le 22 décembre 2022 — Dernière version : 18 juin 2025

% Spécifie que LaTeX2e est requis
\NeedsTeXFormat{LaTeX2e}

% Fournit la classe avec nom et version
\ProvidesClass{BeamerTemplate}

% Charge la classe Beamer avec options par défaut
\LoadClass[
  aspectratio=169,    % Format écran 16:9
  %handout,           % (désactivé ici) Mode sans animation pour impression
  xcolor=table,       % Couleurs étendues, utiles pour les tableaux
  12pt                % Taille de police par défaut
]{beamer}

% Charge des packages conditionnellement selon le moteur utilisé
\RequirePackage{iftex}

\ifXeTeX
  % Configuration pour XeLaTeX : support natif d’Unicode
  \RequirePackage{polyglossia}
  \setdefaultlanguage[frenchpart=false]{french}

  \RequirePackage[math-style=TeX, bold-style=upright]{unicode-math}

  % Polices personnalisées
  \setmainfont{Linux Libertine O}
  \setsansfont{Linux Biolinum O}[Ligatures=TeX,Scale=MatchLowercase]
  \setmathfont{Latin Modern Math}
  \setmathfont[range=\setminus]{XITS Math}  % Pour un beau symbole d’ensemble
\else
  % Configuration pour pdfLaTeX
  \RequirePackage[french]{babel}
  \RequirePackage[T1]{fontenc}
  \RequirePackage[utf8]{inputenc}

  % Mathématiques et polices classiques
  \RequirePackage{amsmath}
  \RequirePackage{amsfonts}
  \usepackage{libertine}                        % Texte avec Libertine
  \usepackage[frenchmath]{libertinust1math}     % Math avec Libertinus
\fi

% Hyperliens cliquables et configurables
\RequirePackage{hyperref}
\hypersetup{
  pdfencoding=auto,
  breaklinks=true,
  colorlinks=true,
  linkcolor=.,
  filecolor=blue,
  urlcolor=blue,
  anchorcolor=.,
  citecolor=blue
}

% Apparence des URLs
\RequirePackage{url}
\renewcommand{\UrlFont}{\scriptsize}

\RequirePackage{csquotes}


% ============================
% === Packages supplémentaires ===
% ============================

\RequirePackage[many]{tcolorbox}   % Pour les boîtes personnalisées (théorèmes, définitions, etc.)
\RequirePackage{fontawesome5}      % Icônes FontAwesome (ex : \faIcon{...})
\RequirePackage{siunitx}           % Formatage des unités (ex : \SI{5}{\kilogram})
\RequirePackage{centernot}         % Permet de centrer les symboles de négation (ex : \centernot\in)

\RequirePackage{pgfplots} % pour certains graphiques tikz
\usetikzlibrary{patterns} % pour certains graphiques tikz
\pgfplotsset{compat=1.18}
\RequirePackage{mathrsfs}
\usetikzlibrary{patterns,shapes,arrows.meta,positioning,calc,intersections,decorations.pathreplacing,backgrounds,fit,plotmarks,pgfplots.fillbetween}
% ================================
% === Mise en valeur de définitions ===
% ================================

%\newcommand{\defn}[1]{%
 % \textbf{\textcolor{Dominante}{#1}}%
%}
% Utilisation : \defn{mot important} le met en gras et en couleur « Dominante »

% ================================
% === Configuration visuelle de Beamer ===
% ================================

\usefonttheme{professionalfonts}       % Utilise les vraies polices installées (pas les polices par défaut de Beamer)

\setbeamersize{sidebar width left=0pt} % Désactive la barre latérale (utile pour les thèmes sans navigation latérale)

\setbeamertemplate{navigation symbols}{} % Supprime les icônes de navigation (en bas à droite)

\setbeamertemplate{footline}[frame number] % Affiche seulement le numéro de diapositive en bas

\AtBeginSection{\frame{\sectionpage}}

% ================================
% === Styles de texte ===
% ================================

\setbeamerfont{frametitle}{size=\large, series=\bfseries} % Titre des diapositives : plus gros et en gras
\setbeamerfont{title}{size=\Large, series=\bfseries}      % Titre de la présentation : encore plus gros et en gras



% ================================
% === Texte en alerte (accentué) ===
% ================================

% Définit la couleur du texte "alerted" dans Beamer (utilisé avec \alert)
\setbeamercolor{alerted text}{fg=burntorange}

% Commande personnalisée pour surligner un mot ou une expression en orange brûlé
\newcommand{\alerter}[1]{%
  \textcolor{burntorange}{#1}%
}

%%%%%%%%%%%%%%%%%%%%%%


\usepackage{subfiles}
\usepackage{verbatim}
\usepackage{fancyvrb}

\usepackage{tabularx}
\usepackage{booktabs}
\usepackage{array}
\usepackage{multirow}
\usepackage{fontawesome5}
\usepackage{booktabs}

% ================================
% === Commande personnalisée pour section avec diapositive ===
% ================================

% Crée une nouvelle section avec une diapo contenant uniquement le titre
\newcommand{\mysection}[1]{%
  \section{#1} % Crée une vraie section (pour navigation, TOC, etc.)
  \begin{frame}
    \begin{sct}{#1} % Affiche le titre dans une boîte personnalisée (à définir ailleurs)
    \end{sct}
  \end{frame}
}


% ================================
% === Page d’ouverture de section personnalisée ===
% ================================

% Définition de la couleur utilisée pour la boîte de section
\setbeamercolor{colsection}{fg=Dominante!20, bg=Dominante}

% Modèle de page de section (affichée automatiquement à chaque \section)
\setbeamertemplate{section page}{
  \begin{centering}
    \begin{beamercolorbox}[sep=12pt, center]{colsection}
      \usebeamerfont{section title}\insertsection\par % Affiche le titre de la section
    \end{beamercolorbox}
  \end{centering}
}



% ================================
% === Définition des couleurs personnalisées ===
% ================================

% Couleurs générales
\definecolor{burntorange}{RGB}{242, 96, 53}      % Orange vif — utilisé pour le texte d’alerte
\definecolor{Fond}{RGB}{252, 252, 252}           % Couleur d’arrière-plan des diapositives
\definecolor{TitreDiapo}{RGB}{0, 10, 0}          % Couleur du titre des diapositives

% Couleurs thématiques principales
\definecolor{Dominante}{RGB}{125, 195, 229}      % Couleur principale (bleu clair)
\definecolor{Accent}{RGB}{239, 131, 84}          % Couleur secondaire (orange accentué)

% Couleurs des boîtes personnalisées
\definecolor{TexteBoite}{RGB}{25, 50, 102}         % Texte des boîtes \Boite
\definecolor{TitreBoite}{RGB}{25, 50, 102}         % Titre des boîtes \Boite

% Couleurs des théorèmes
\definecolor{TexteThm}{RGB}{0, 0, 0}             % Texte des théorèmes
\definecolor{TitreThm}{RGB}{0, 0, 0}             % Titre des théorèmes

% Couleur des items de listes
\definecolor{itemizecolor}{RGB}{0, 0, 0}

% Couleurs institutionnelles (Université Laval)
\definecolor{rougelaval}{RGB}{237, 28, 36}        % Rouge ULaval
\definecolor{jaunelaval}{RGB}{251, 199, 7}        % Jaune ULaval

% ================================
% === Alias de couleurs pour les boîtes tcolorbox ===
% ================================

% Définitions
\colorlet{DefnColorBar}{Dominante}        % Barre latérale des définitions
\colorlet{DefnColorBG}{Dominante!10}      % Fond des définitions

% Théorèmes
\colorlet{ThmColorBar}{Accent}            % Barre latérale des théorèmes
\colorlet{ThmColorBG}{Accent!20}          % Fond des théorèmes

% Propositions
\colorlet{PropColorBar}{Accent}           % Barre latérale des propositions
\colorlet{PropColorBG}{Accent!20}         % Fond des propositions

% ================================
% === Application des couleurs dans Beamer ===
% ================================

% Couleur d’arrière-plan générale
\setbeamercolor{background canvas}{bg=Fond}
\setbeamercolor{normal text}{bg=Fond, fg=black}

% Titres de diapositives
\setbeamercolor{frametitle}{fg=TitreDiapo, bg=Fond}

% Blocs standards
\setbeamercolor{block body}{bg=normal text.bg!90!black}
\setbeamercolor{block title}{bg=black!60, fg=white}
\setbeamercolor{block title example}{fg=example text.fg!75!normal text.fg, bg=Fond}

% Éléments de structure (ex. titres de sections, numérotation)
\setbeamercolor{structure}{bg=black, fg=white}

% Listes à puces et numérotées
\setbeamercolor{itemize item}{fg=itemizecolor}
\setbeamercolor{enumerate item}{fg=itemizecolor}
\setbeamercolor{itemize subitem}{fg=itemizecolor}
\setbeamercolor{itemize subsubitem}{fg=itemizecolor}


% ================================
% === Styles pour les listes ===
% ================================

% Style des puces de listes
\setbeamertemplate{itemize item}[circle]       % Puce principale : cercle
\setbeamertemplate{itemize subitem}[square]    % Sous-puce : carré
\setbeamertemplate{itemize subsubitem}[triangle] % Sous-sous-puce : triangle

% ================================
% === Boîtes personnalisées (tcolorbox) ===
% ================================

% Définition : boîte pour les définitions
\newtcolorbox[auto counter]{defn}[1][]{
  boxrule=0pt,
  boxsep=0pt,
  colback=DefnColorBG,             % Fond
  enhanced jigsaw,
  detach title,
  fonttitle=\bfseries,
  coltitle=black,                  % Titre en noir
  title=Définition.~,
  before upper={\tcbtitle},
  borderline west={2pt}{0pt}{DefnColorBar}, % Barre colorée à gauche
  sharp corners,
  before skip=10pt,
  after skip=10pt,
  breakable,
}

% Théorème : boîte pour les théorèmes
\newtcolorbox[auto counter]{thm}[1][]{
  boxrule=0pt,
  boxsep=0pt,
  colback=ThmColorBG,
  enhanced jigsaw,
  detach title,
  fonttitle=\bfseries,
  coltitle=TitreThm,
  coltext=TexteThm,
  title=Théorème.~,              % #2 = nom ou numéro
  before upper={\tcbtitle},
  borderline west={2pt}{0pt}{ThmColorBar},
  sharp corners,
  before skip=10pt,
  after skip=10pt,
  breakable,
}

% Proposition : boîte pour les propositions
\newtcolorbox[auto counter]{prp}[1][]{
  boxrule=0pt,
  boxsep=0pt,
  colback=PropColorBG,
  enhanced jigsaw,
  detach title,
  fonttitle=\bfseries,
  coltitle=black,
  title=Proposition.~,           % #2 = nom ou numéro
  before upper={\tcbtitle},
  borderline west={2pt}{0pt}{PropColorBar},
  sharp corners,
  before skip=10pt,
  after skip=10pt,
  breakable,
}

% Boîte « Boite » : encadré informatif ou pédagogique
\newtcolorbox[auto counter]{Boite}[2][]{
  boxrule=0pt,
  boxsep=0pt,
  colback=Dominante!20,
  collower=Fond,
  enhanced jigsaw,
  fonttitle=\medskip\bfseries,
  coltitle=TitreBoite,
  coltext=TexteBoite,
  title=#2,
  colbacktitle=Dominante,
  before upper={\tcbtitle},
  borderline west={2pt}{0pt}{Dominante},
  sharp corners,
  before skip=10pt,
  after skip=10pt,
  bottomtitle=1mm,
  toptitle=1mm,
  breakable,
}

% ================================
% === Gabarit personnalisé pour la page de titre ===
% ================================

\defbeamertemplate*{title page}{titlepage}{
  \thispagestyle{empty}

  % Ligne décorative en arrière-plan centrée horizontalement
  \begin{tikzpicture}[remember picture, overlay]
    \node[anchor=center, inner sep=0pt] at ([yshift=0cm]current page.center)
      {\includegraphics[width=\paperwidth]{ligne2.png}};
  \end{tikzpicture}

  \vspace*{0.5cm}


\begin{center}
        % Titre et sous-titre
    {\Large \color{black} \inserttitle\par}
    \vspace{0.2cm}
    { \color{black!67} \insertsubtitle\par}



\vspace{2cm}  % Remplit tout l’espace au-dessus
    

      \insertinstitute

      \vspace{1cm}
      
      \includegraphics[width=4cm]{logo_UL.pdf}



  \end{center}

}


% ================================
% === Configuration de la bibliographie avec biblatex ===
% ================================

\RequirePackage[
  backend=biber,         % Utilise Biber comme moteur
  style=numeric-comp,    % Style numérique compact (1,2,3 devient 1–3)
  sorting=none           % Pas de tri : l’ordre d’apparition est conservé
]{biblatex}

\addbibresource{reference.bib}  % Fichier de références

% Couleurs pour les entrées de bibliographie
\setbeamercolor{bibliography entry author}{fg=black}
\setbeamercolor{bibliography entry location}{fg=black}
\setbeamercolor{bibliography entry note}{fg=black}
\setbeamercolor{bibliography item}{fg=black}

% ================================
% === Métadonnées de la présentation ===
% ================================

\subtitle{STT-2902 Modélisation statistique}
\institute{Département de mathématiques et de statistique}