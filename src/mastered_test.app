{application,mastered_test,
             [{description,[]},
              {vsn,"0.0.1"},
              {applications,[ezk]},
              {mod,{mastered_test_app,[]}},
              {env,[
              			{orders, [1, 4]},
              			{number_of_workers, 2},
              			{address, "local"}
              		]
              },
              {modules,[mastered_test,mastered_test_app,worker]}]}.
