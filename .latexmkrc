ensure_path('TEXINPUTS', './/' . ':');

# Force mode: complete the build even when there are multiply-defined
# label warnings (e.g. from the exam class internal part labels).
$force_mode = 1;
$max_repeat = 3;

# Ensure the workspace root (where Evaluation.cls and BeamerTemplate.cls live)
# is always in the search path, regardless of cwd.
# Walk up from the current directory to find the root containing Evaluation.cls.
use Cwd 'abs_path';
my $dir = abs_path('.');
while ($dir ne '/' && ! -f "$dir/Evaluation.cls") {
    $dir =~ s{/[^/]+$}{};
}
if (-f "$dir/Evaluation.cls") {
    ensure_path('TEXINPUTS', $dir . '//' . ':');
}
