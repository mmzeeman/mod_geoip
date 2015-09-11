{% extends "admin_base.tpl" %}

{% block title %} {_ Admin GeoIP _} {% endblock %}

{% block content %}
{% with m.acl.is_admin as is_editable %}
<div class="admin-header">
    <h2>{_ GeoIP _}</h2>

    <p>{_ Manage your GeoIP Settings _}</p>
</div>

<div class="row">
    <div class="col-lg-8 col-md-6">
        <p>This product includes GeoLite2 data created by MaxMind, available from</p>
        <a href="http://www.maxmind.com">http://www.maxmind.com</a>.
    </div>

   {% button class="btn btn-primary" text=_"Refresh Maxmind GeoIP databases" action={maxmind_download} %} 
</div>

{% endwith %}
{% endblock %}
