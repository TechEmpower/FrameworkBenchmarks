module HelloWorld
  module Repos
    class FortuneRepo < HelloWorld::DB::Repo
      def all
        results = fortunes.to_a.map(&:to_h)
        results << { id: 0, message: 'Additional fortune added at request time.' }
        results.sort_by! { |fortune| fortune[:message] }
      end
    end
  end
end
