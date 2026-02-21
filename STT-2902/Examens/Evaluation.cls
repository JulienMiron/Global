\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Evaluation}
%\LoadClass[answers]{exam}
\LoadClass[]{exam}
\def\Gradescope{} % On ne met rien dans les {} pour la version à imprimer. On met quelque chose (par exemple {.}) pour la version Gradescope, c'est-à-dire en recto seulement pour l'importation du pdf dans Gradescope
\RequirePackage{iftex}
\RequirePackage{geometry}
\RequirePackage{enumitem}
\RequirePackage{mathtools}
\RequirePackage[many]{tcolorbox}
\usepackage{xfrac}
\usepackage{diagbox}

\usepackage{comment}
\usepackage{afterpage}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{ifsym}
\usepackage{multirow} 
\usepackage{icomma}
\usepackage{array}
\usepackage[9pt]{extsizes}
\usepackage{arev}
\usepackage[T1]{fontenc}
\usepackage{tablefootnote}

\newcolumntype{P}[1]{>{\centering\arraybackslash}p{#1}}

\newcommand\blankpage{
    \null
    \thispagestyle{empty}
    \addtocounter{page}{-1}
    \newpage
    }

\RequirePackage[french]{babel}

    
\RequirePackage{xcolor}
\RequirePackage{etoolbox}
\RequirePackage{calc}
\RequirePackage{tikz}

\usetikzlibrary{arrows.meta,calc,positioning,decorations.markings}
\tikzset{
  >={Latex[length=2.2mm]},
  dot/.style = {circle,draw,fill=white,inner sep=1.1pt,minimum size=3.8pt},
  open/.style= {circle,draw,fill=white,inner sep=1.1pt,minimum size=4pt},
  pt/.style  = {font=\small},
}

\tikzset{
mid arrow/.style={
    postaction={decorate},
    decoration={markings, mark=at position 0.5 with {\arrow{Latex}}}
  }
}
  
\usepackage{listings}

\lstset{language=R,  basicstyle=\ttfamily}


%%%% Modifie le symbole des choix de réponse à cocher
\checkboxchar{
\raisebox{-0.1cm}{
    \begin{tikzpicture}
    \draw (0,0) -- (0.5,0) -- (0.5,0.5) -- (0,0.5) -- cycle;
    \end{tikzpicture}}}

%%%% Lorsqu'une question se continue sur une autre page on utilise 
%%%% \pagesuivante{\ref{LABEL DE l'exercice}}
%%%% et 
%%%% \suite{\ref{LABEL DE l'exercice}}
\newcommand{\pagesuivante}[1]{
\begingroup
    \large 
    \emph{\textbf{Suite de l'exercice #1 sur la feuille suivante}}
    \medskip
    \endgroup
}
\newcommand{\suite}[1]{
    \begingroup
     \large \emph{\textbf{Suite de l'exercice #1}}
    \endgroup
}

\newcommand{\Mypm}{\mathbin{\tikz [x=1.4ex,y=1.4ex,line width=.1ex] \draw (0.0,0) -- (1.0,0) (0.5,0.08) -- (0.5,0.92) (0.0,0.5) -- (1.0,0.5);}}%


    \newcommand{\boite}[2]
		{
		\raisebox{-.4cm}{
				\begin{tikzpicture}
					\draw (0,0)--(#1,0) -- (#1,#2) -- (0,#2) -- cycle;		
				\end{tikzpicture}
            }
        }

\def\checkmark{\tikz\fill[scale=0.4](0,.35) -- (.25,0) -- (1,.7) -- (.25,.15) -- cycle;} 


\newcommand{\carresection[1]}
{
	\begin{tikzpicture}
	\draw (0,0) -- (0.5,0) -- (0.5,0.5) -- (0,0.5) -- cycle;
	\draw (0.5,0.25) node[align=left, right]{#1};
	\end{tikzpicture}
}
\newcommand{\separation}
{
	\centering
	\begin{tikzpicture}[scale=0.33]
	%\draw[thick] (0,0) -- (1,0);
	\draw (0,0) .. controls (1,1) and (2,-1) .. (3,0);
	\draw[fill=black] (3.5,0) circle (0.3cm);
	\draw (4,0) .. controls (5,-1) and (5,1) .. (7,0);
	%\draw[thick] (2,0) -- (3,0);
	\end{tikzpicture}
}
\newcommand{\myline}[1]
{
	\mathrel{\raisebox{-.7ex}{\begin{tikzpicture}
			\draw (0,0)--(#1,0);
			\end{tikzpicture}}}
}

\providecommand{\SectionD}{}
\providecommand{\SigleNumero}{}
\providecommand{\NomCours}{}
\providecommand{\DateEvaluation}{}
\providecommand{\HeureEvaluation}{}
\providecommand{\Session}{}
\providecommand{\NomEvaluation}{}
\providecommand{\Ponderation}{}
\providecommand{\SectionA}{}
\providecommand{\SectionB}{}
\providecommand{\SectionC}{}
\providecommand{\SectionD}{}
\providecommand{\Matricule}{}
\providecommand{\Coordonnateur}{}
\providecommand{\Coordonnatrice}{}
\providecommand{\Enseignant}{}
\providecommand{\Directives}{}
\providecommand{\NombreLignes}{}
\providecommand{\PapierLegal}{}
\providecommand{\EnLigne}{}
\providecommand{\Solutions}{}
\providecommand{\Gradescope}{}


\newcommand{\pageblanche}{
\ifdefempty{\Gradescope}{\newpage}{
\newpage
\thispagestyle{empty}
\null
\newpage
}
}




\setlength\linefillheight{1cm}

% Précise le format des en-têtes et pieds de page
\firstpageheader{}{}{}


\runningheader{}
	{}
	{}

\runningfooter{\small Page \thepage~/~\numpages }
	{\small\ifdefempty{\SigleNumero}{}{\hspace{.58cm}\SigleNumero\small\ifdefempty{\NomEvaluation}{}{~--~\NomEvaluation}}}
	{
        \begin{tikzpicture}
        \draw (0,0) -- (1.5,0) -- (1.5,0.9) -- (0,0.9) -- cycle;
        \draw (0,0.35) node[left]{Vos initiales:~};
        \end{tikzpicture}}
        
	%\noboxedpoints
	%\nopointsinmargin
 
	\qformat{\hspace{.58cm}{\sc\textbf{Exercice~\thequestion}}~{\small(\,\totalpoints~points)}~\hfill}
	
	\bonusqformat{\hspace{.58cm}{\sc\textbf{Exercice~\thequestion{} BONUS}}~{\small(\,\totalbonuspoints~points)}~\hfill}
	\bonuspointpoints{bonus point}{points}
 
	\addpoints
 
	\newcommand{\couverture}{
	\newgeometry{margin=2cm}
		\begin{minipage}[t]{7cm}
			\begin{flushleft}
				\hspace{-.5cm}\includegraphics[width=5cm]{logo_ul.pdf}\\
				\hspace{-.5cm}{\sf\scriptsize \textbf{Faculté des sciences et génie}}\\[-0.2cm]
				\hspace{-.5cm}{\sf\scriptsize Département de mathématiques et statistique}\\[2cm]
			\end{flushleft}
		\end{minipage}~~~
		\begin{minipage}[t]{\textwidth-5.5cm}
			\begin{tikzpicture}
			\draw (0,0) -- (10,0) -- (10,1.5) -- (0,1.5) -- cycle;
			\draw (0,0) node[below right]{\scriptsize\text{Prénom et nom}};
			\end{tikzpicture}\\[0.4cm]
			\begin{tikzpicture}
			\draw (0,0) -- (9,0) -- (9,1) -- (0,1) -- cycle;
			
			\draw (0,0) node[below right]{\scriptsize\text{NI (9 chiffres)}};
			\end{tikzpicture}\\[0.4cm]
			\ifdefempty{\SectionA}{}{\carresection[\SectionA]\\}
			\ifdefempty{\SectionB}{}{\carresection[\SectionB]\\}
			\ifdefempty{\SectionC}{}{\carresection[\SectionC]\\}
			\ifdefempty{\SectionD}{}{\carresection[\SectionD]}
			\ifdefempty{\SectionA}{}{\scriptsize\text{Cochez le nom de votre enseignant}}
		\end{minipage}
		\vfill
		\begin{center}
			{\huge\textbf{\ifdefempty{\NomEvaluation}{}{\NomEvaluation}\ifdefempty{\Ponderation}{}{~(\Ponderation~\%)}}}~\\
			{\Large\ifdefempty{\NomCours}{}{\NomCours}\ifdefempty{\SigleNumero}{}{~(\SigleNumero)}}~\\
			{\Large\ifdefempty{\DateEvaluation}{}{{\DateEvaluation}}}~\\
   \smallskip
   {\LARGE\ifdefempty{\HeureEvaluation}{}{{\HeureEvaluation}}}~\\

			\ifdefempty{\Enseignant}{}{Enseignant : \Enseignant}\ifdefempty{\Enseignante}{}{Enseignante : \Enseignante}~\\
			\ifdefempty{\Coordonnateur}{}{Coordonnateur : \Coordonnateur}\ifdefempty{\Coordonnatrice}{}{Coordonnatrice : \Coordonnatrice}
		\end{center}
		\vfill

		\ifdefempty{\Directives}{}{{\noindent\textbf{Directives de l'évaluation} \Directives}}
		\medskip
		\newgeometry{margin=1.7cm,left=0.7cm,bottom=1.5cm,footskip = 0mm}
	}

\ProcessOptions

\AtBeginDocument{\couverture}