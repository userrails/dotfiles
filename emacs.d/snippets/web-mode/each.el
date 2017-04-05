# -*- mode: snippet -*-
# name: <% obj.each .... %>...<% end %>
# key: each
# --
<% ${1:obj}.each do |${2:key}, ${3:value}| %>
   $0
<% end %>