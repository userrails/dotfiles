# -*- mode: snippet -*-
# name: it { is_expected.to validate_uniqueness_of .. }
# key: vuo
# --
it { is_expected.to validate_uniqueness_of :$0 }