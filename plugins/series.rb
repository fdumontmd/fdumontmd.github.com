module Jekyll
  class Post
    alias_method :old_render, :render
    alias_method :old_to_liquid, :to_liquid
    
    def series_posts(posts)
      if self.data.has_key?("series")
        (posts.find_all {|post| post.data["series"] == self.data["series"] }).reverse
      else
        []
      end
    end
    
    def short_title()
      if self.data.has_key?("short_title")
        self.data["short_title"]
      elsif self.data.has_key?("series") and self.data.has_key?("title") and self.data["title"].start_with?(self.data["series"])
        self.data["title"][self.data["series"].length, self.data["title"].length].strip
      else
        self.data["title"]
      end
    end
    
    def render(layouts, site_payload)
      payload = {
        "site" => { "series_posts" => series_posts(site_payload["site"]["posts"])
          }
      }.deep_merge(site_payload)
      old_render(layouts, payload)
    end
    
    def to_liquid
      old_to_liquid.deep_merge({
        "short_title" => short_title()
      })
    end
  end
end