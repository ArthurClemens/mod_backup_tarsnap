{#
params:
archives
#}
<table class="table">
    <thead>
        <tr>
            <th>{_ Archive name _}</th>
            <th>{_ Job _}</th>
            <th>{_ Date _}</th>
        </tr>
    </thead>
    <tbody>
        {% for archive in archives %}
            <tr>
                <td>
                    {{ archive.archive }}
                </td>
                <td>
                    {{ archive.job }}
                </td>
                <td>
                    {{ archive.date|date:"M d Y, H:i:s" }}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="3">
                    {_ No archives present. _}
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
