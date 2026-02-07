module HelloWorld
  module Views
    module Fortunes
      class Index < HelloWorld::View

        include Deps["repos.fortune_repo"]

        expose :fortunes do
          fortune_repo.all
        end
      end
    end
  end
end
