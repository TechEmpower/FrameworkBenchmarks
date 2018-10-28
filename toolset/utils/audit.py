from toolset.utils.output_helper import log
from colorama import Fore


class Audit:
    '''
    Audits frameworks for inconsistencies
    '''

    def __init__(self, benchmarker):
        self.benchmarker = benchmarker

    def start_audit(self):
        for lang in self.benchmarker.metadata.gather_languages():
            for test_dir in self.benchmarker.metadata.gather_language_tests(
                    lang):
                self.audit_test_dir(test_dir)

    def audit_test_dir(self, test_dir):
        warnings = 0
        log('Auditing %s:' % test_dir, color=Fore.BLUE)

        if not self.benchmarker.metadata.has_file(test_dir, 'README.md'):
            log('README.md file is missing')
            warnings += 1

        if warnings:
            log('(%s) warning(s)' % warnings, color=Fore.YELLOW)
        else:
            log('No problems to report', color=Fore.GREEN)
