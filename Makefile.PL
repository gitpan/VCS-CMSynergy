use ExtUtils::MakeMaker;

WriteMakefile(
    NAME		=> 'VCS::CMSynergy',
    VERSION_FROM	=> 'VCS/CMSynergy.pm',
    PREREQ_PM		=> {
    #			     'Expect'		=> '1.15',
			     'File::Spec'	=> 0,
			     'File::Temp'	=> 0,
			     'Memoize'		=> 0,
			   },
    dist		=> { 
			     COMPRESS => 'gzip -9f', 
			     SUFFIX => 'gz' 
			   },
);
