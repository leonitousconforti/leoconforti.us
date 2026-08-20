---
title: SkyPortal release 2026-08-20

description: All the skyportal changes merged during the week of
  2026-08-13 to 2026-08-20

tags: skyportal, python, news

toc: true
---

This week included another plethora of bug fixes, web interface updates, and
quality of life updates!

## Changes You Might See
* Show associated objs and previous mag in scanning reports by @thomasculino in https://github.com/skyportal/skyportal/pull/6528
* better fill table space by @mcoughlin in https://github.com/skyportal/skyportal/pull/6531
* track solar system objects as sources by @mcoughlin in https://github.com/skyportal/skyportal/pull/6530
* Add walkthrough for filter builder by @thomasculino in https://github.com/skyportal/skyportal/pull/6533
* Move broker tab up the sidebar by @thomasculino in https://github.com/skyportal/skyportal/pull/6536
* sortable observation run columns by @mcoughlin in https://github.com/skyportal/skyportal/pull/6534
* Add sidebar alerts shortcut to default alert broker. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6538
* Add one-time feature announcement popups. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6544
* Add feature announcement steps for new alerts and brokers pages. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6545
* update existing classifications with the sliders by @mcoughlin in https://github.com/skyportal/skyportal/pull/6548
* config-driven branding for login and About pages by @mcoughlin in https://github.com/skyportal/skyportal/pull/6546
* boom survey tooltip by @mcoughlin in https://github.com/skyportal/skyportal/pull/6555
* Refactor sidebar component for improved drawer behavior and styling. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6557
* Improve comments on source page by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6495
* Candid thumbnail by @mcoughlin in https://github.com/skyportal/skyportal/pull/6561
* Add links formatting by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6560
* add groups saved to and fix other surveys detections by @thomasculino in https://github.com/skyportal/skyportal/pull/6566
* empty observation page by @mcoughlin in https://github.com/skyportal/skyportal/pull/6568
* test session teardown by @mcoughlin in https://github.com/skyportal/skyportal/pull/6556
* Update drawer width for improved layout responsiveness. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6569
* Improve user management page by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6562
* Fix boom mongo dialog rawpipeline by @thomasculino in https://github.com/skyportal/skyportal/pull/6572
* Add interested button by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6550

## Changes Behind the Scenes
* automate superobject creation by @mcoughlin in https://github.com/skyportal/skyportal/pull/6532
* Drop unrecognized filter rows in ATLAS forced photometry ingestion by @thomasculino in https://github.com/skyportal/skyportal/pull/6537
* make filter autosave settable and consistent by @mcoughlin in https://github.com/skyportal/skyportal/pull/6535
* finish anyOf → enum transition by @mcoughlin in https://github.com/skyportal/skyportal/pull/6542
* replace the confirmed boolean and gcn_crossmatch array with GcnEventObj.status by @mcoughlin in https://github.com/skyportal/skyportal/pull/6540
* Make tests reliable and faster by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6549
* EP permissions by @mcoughlin in https://github.com/skyportal/skyportal/pull/6547
* MPC query readability by @mcoughlin in https://github.com/skyportal/skyportal/pull/6541
* Parallelize broker ingestion via Kafka consumer groups and multi-process broker_ingest by @mcoughlin in https://github.com/skyportal/skyportal/pull/6552
* pin baselayer to 32bb65cddd27f04c1c9552aacd53fcfeb1d9785b by @mcoughlin in https://github.com/skyportal/skyportal/pull/6554
* Fix object query on broker alerts page by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6553
* Fix lost update on profile preferences by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6559
* Type path parameters from handler signatures by @leonitousconforti in https://github.com/skyportal/skyportal/pull/6551
* Add broker_ingest supervisor.conf to .gitignore. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6558
* other auth backends by @mcoughlin in https://github.com/skyportal/skyportal/pull/6523
* sentry august 18 by @mcoughlin in https://github.com/skyportal/skyportal/pull/6564
* backend source-list query cache by @mcoughlin in https://github.com/skyportal/skyportal/pull/6565
* add per-call timeout for BOOM queries by @mcoughlin in https://github.com/skyportal/skyportal/pull/6570
* hand the auth pipeline the association a matched sign-in belongs to by @mcoughlin in https://github.com/skyportal/skyportal/pull/6574
* Avoid 2h ci timeout by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6579
* infer the survey from the object id when saving a broker alert. by @antoine-le-calloch in https://github.com/skyportal/skyportal/pull/6577
* Fix query param parsing for autosaveGroupIds and hasTNSname by @leonitousconforti in https://github.com/skyportal/skyportal/pull/6571
* empty query warning for lasair by @mcoughlin in https://github.com/skyportal/skyportal/pull/6581

## Full Changelog

The full changelog is available at
https://github.com/skyportal/skyportal/compare/2026-08-13...2026-08-20
