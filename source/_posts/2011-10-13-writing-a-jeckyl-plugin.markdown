---
layout: post
title: "Writing a Jeckyl Plugin"
date: 2011-10-13 12:40
comments: true
categories: Blog
tags: ruby, jekyll
---
This blog is built with [Octopress](http://octopress.org/), itself built on top of [Jekyll](http://jekyllrb.com/), a static site generator. 
<!--more-->
As I was blogging through [Seven Languages in Seven Weeks]({{ root_url }}/blog/2011/10/11/seven-languages-in-seven-weeks/), I thought it would be neat to have a list of all the posts in that series, accessible from the sidebar.

Essentially, my goal was to be able to add an attribute `series` to a post, to add it to a named group of posts, and to generate a list of all posts in the same group to add to the sidebar.

The first part is easy. In Jekyll, the [YAML Front Matter](https://github.com/mojombo/jekyll/wiki/YAML-Front-Matter) is extensible. So I just added 

```
series: "Seven Languages in Seven Weeks"
```

to the relevant posts.

The second one is a bit trickier. Jekyll exposes the post data not directly, but through a hash that is built for each post. This hash is built by two methods, `render` and `to_liquid`.

My plugin replaces `render` to insert the `series_posts`, an array of all the posts with the same `series` attribute.

But to display the list of posts, I need a short title, so also added support for that. A short title is computed in the overridden `to_liquid` method. The computation is the following:

 *  if there is a short_title attribute in the YAML Front Matter, use it
 *  otherwise, if the name of the series is a prefix of the post title, remove it from the title and use the rest
 *  otherwise use the post title.

To display the series posts in the sidebar, I'm using this code:
{% include_code series.html lang:html %}

The code can be found [here](https://github.com/fdumontmd/jekyll-plugins/tree/master/series).
