    use ExtUtils::MakeMaker;
    WriteMakefile
      ('DISTNAME' => 'Bot-BasicBot',
       'VERSION' => '0.2',
       'PL_FILES' => {},
       'PREREQ_PM' => {
			'Pod::Coverage' => '0',
			'POE' => '0.22',
			'Test::More' => '0',
			'POE::Component::IRC' => '2',

		      },
      );
