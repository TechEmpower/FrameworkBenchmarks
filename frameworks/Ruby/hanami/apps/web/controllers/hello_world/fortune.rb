module Web::Controllers::HelloWorld
  class Fortune
    include Web::Action
    expose :fortunes

    def call(params)
      self.headers.merge!({ 'Content-Type' => 'text/html; charset=utf-8' })
      @fortunes = FortuneRepository.new.all
      @fortunes << ::Fortune.new(id: 0, message: "Additional fortune added at request time.")
      @fortunes = @fortunes.sort_by { |x| x.message }
    end
  end
end
