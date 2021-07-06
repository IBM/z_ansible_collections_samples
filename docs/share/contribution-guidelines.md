# Contribution Guidelines for the Ansible for IBM Z Playbook Repository

- [Developer Certificate of Origin](#developer-certificate-of-origin)
    - [Useful tools to make doing DCO signoffs easier](#useful-tools-to-make-doing-dco-signoffs-easier)
    - [Signoff for commits where the DCO signoff was missed](#signoff-for-commits-where-the-dco-signoff-was-missed)
    - [Handling DCO errors using GitHub website commits](#handling-dco-errors-using-github-website-commits)


This document captures the general guidelines for contributing to the Ansible for IBM Z Playbooks repository. All playbooks contributed to this repository are required to follow these guidelines.

## Developer Certificate of Origin

The Ansible for IBM Z playbook repository requires the use of the [Developer’s Certificate of Origin 1.1 (DCO)](https://developercertificate.org/), which is the same mechanism that the [Linux® Kernel](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/Documentation/process/submitting-patches.rst#n416) and many other communities use to manage code contributions. The DCO is considered one of the simplest tools for sign offs from contributors as the representations are meant to be easy to read and indicating signoff is done as a part of the commit message.

Here is an example Signed-off-by line, which indicates that the submitter accepts the DCO:

<code>Signed-off-by: John Doe <john.doe@hisdomain.com></code>

You can include this automatically when you commit a change to your local git repository using <code>git commit -s</code>.

### Useful tools to make doing DCO signoffs easier

There are a number of great tools out there to manage DCO signoffs for developers to make it much easier to do signoffs.

- DCO command line tool, which let's you do a single signoff for an entire repo ( https://github.com/coderanger/dco )
- GitHub UI integrations for adding the signoff automatically ( https://github.com/scottrigby/dco-gh-ui )
  - Chrome - https://chrome.google.com/webstore/detail/dco-github-ui/onhgmjhnaeipfgacbglaphlmllkpoijo
  - Firefox - https://addons.mozilla.org/en-US/firefox/addon/scott-rigby/?src=search

Additionally, it is possible to use shell scripting to automatically apply signing. Here is an example for bash, to be put into a .bashrc file:

```
git() {
    if [[ $1 == "commit" ]]; then
        shift
        echo "Executing git commit -s $@"
        command git commit -s "$@"
    else
        command git "$@"
    fi
}
```

### Signoff for commits where the DCO signoff was missed

When bringing in a code repository for the first time, or commits done before the DCO checks are enabled, there would be a series of commits that don't include the sign-off statement. You can retroactively signoff commits you've made by make a commit with your DCO signoff that contains a new text file ( suggested name is past_commits.txt ) with the following contents:

````
The following commits were made pursuant to the Developer Certificate of Origin, even though a Signed-off-by: was not included in the commit message.

<COMMIT HASH> <COMMIT MSG>
...
````

Each user who has made the past commits should have thier own <code>Signed-off-by:</code> line in the commit message.

This process can be automated using the [DCO Org Check script](https://github.com/jmertic/dco-org-check).

### Handling DCO errors using GitHub website commits

The [Probot: DCO](https://github.com/probot/dco) app requires that the email address and name specifyed in the DCO Signoff match that of the current infortmation from the user making the commit. Generally this is handled automatically when using a local git client, but when making contributions from the GitHub website directly this needs to be aligned manually. 

If you are using one of the recommended [GitHub UI integrations for adding the signoff automatically]( https://github.com/scottrigby/dco-gh-ui), you will want to ensure that the name and email listed there match that which is in your GitHub profile.
