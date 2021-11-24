# Acceptance tests for cl-webcat

Checklist for making sure it actually works.

Follow the steps in order, as these are not discrete unit tests; later steps assume that the earlier ones have been done.


# Upload the webcat schema

- `curl -i --data-urlencode schema@/path/to/webcat.json -X POST http://192.0.2.1:4965/schema/v1/`


# Create regular Resources

- Create one each of the following resources:
    - `Tags` with UID `someTag`
    - `Groups` with UID `someGroup`
    - `People` with UID `thisPerson`
- Each operation should return a display view for the newly-created resource, which includes one entry under "Outbound links" of `/CREATOR/People/RgAdmin`.

Note: the UIDs are not meaningful. It's just simpler to be specific than to repeatedly say "the `Tag` resource you created in the first step."


# Search

- Search for the tag, group and person you just created, confirming that the results are no more and no less than expected.
- Search for all `People`
    - This should return both `RgAdmin` and `thisPerson`


# Add tags and groups

- Click on `thisPerson`
- Click on the "Edit tags, groups and links" link at the bottom of the page.
- Select the "thisTag" tag in "Add tags" and the "thisGroup" group in "Add groups".
- Click on "Add tags and groups".
- You should now see a display page for "People thisPerson" with the tag and group you just added.
- Confirm that neither the tag nor the group appear in the Outbound Links list


# Search with tags and groups

- Search for a `People` resource with the `thisTag` tag.
- There should be one result (`thisPerson`).
    - `RgAdmin` should _not_ be included in the results.
- Search for a `People` resource with the `thisGroup` group.
- There should be one result (`thisPerson`).
    - `RgAdmin` should _not_ be included in these results either.


# Link to another resourcetype

- Open a new tab.
- In the new tab, create an `Organisations` resource with UID `MyCorp`
- In the first tab, click on the `thisPerson` link to view the resource, then click on the "Edit tags, groups and links" link at the bottom of the page.
- Type (or copy-paste) "/MEMBER_OF/Organisations/MyCorp" into the "Link to other resources" text box, and click on "Update tags and groups".
- You should now see the `display/People/thisPerson` page, with two links in the "Outbound Links" section
    - `/MEMBER_OF/Organisations/MyCorp`
    - `/CREATOR/People/RgAdmin`
- Click on the `/MEMBER_OF/Organisations/MyCorp` link, and confirm that it takes you to a valid page displaying that resource.
- You can close the extra tab now, as it's no longer useful.


# Edit a resource

- On the `MyCorp` page, click on `Edit` in the navigation bar at the top of the page.
- In the "Edit attributes" page that should result from that, type a description into the text box for that attribute, then click on Update.
- You should now see an updated display page for `MyCorp`, complete with the new description.


# Files

- Click on the "Files" link in the nav-bar.
- Upload an image file.
    - I specify "image" here because this plays into the picture-gallery section below.
- Tag the file with `thisTag`


# Image gallery

- Click on "Image gallery" in the nav-bar.
- You should get the image-gallery page, with the picture you just uploaded.
- Create a new tag with UID `thatTag`
- Back in the gallery, filter for `thisTag`; you should get the same picture.
- Now filter for `thatTag`; you should get the gallery page with _no_ images.


# Tasks

- Click on the `Tasks` link in the navigation bar at the top.
- You _should_ see a search-page for tasks, with an empty results section.
- Create a task. Tag it.
- Go back to the Tasks page. Now you should see a single result.
- Test each of the filters, individually and in combination:
    - tags
    - UID regex
    - Importance
    - Urgency
    - Scale
    - Status


# Files

- Upload a file
- Confirm that you're redirected to a page of metadata for that file.
- Load the gallery page, and confirm that this image is displayed there.
- Check the link from the gallery.
- Try to download the file. Confirm that it comes down intact.


# Other things to check

- Display wikipages (seems to be working)
