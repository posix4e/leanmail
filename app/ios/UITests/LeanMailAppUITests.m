// LeanMail's UI test against a bare development server (DEV_AUTH_BYPASS=1, the fixture mail imported):
// sign in with the dev account, see the inbox, open a message, read its text, mark it done.
//
//   LEANMAIL_TEST_ORIGIN=http://127.0.0.1:8080 LEANMAIL_SCREENSHOTS=<dir> PREPARE_ONLY=1 \
//     APP_DIR=app APP_ROOT=LeanMailApp SCHEME=LeanMailApp app/.lake/packages/leannative/framework/native/ci/build-ios Debug iossim
//   xcodebuild test -project app/ios/LeanMailApp.xcodeproj -scheme LeanMailApp -destination 'platform=iOS Simulator,name=iPhone 17 Pro'
//
// The variables reach the generated scheme's test environment (LeanMailApp/Project.lean), never a tracked file.
#import <XCTest/XCTest.h>
#import "LNTest.h"

@interface LeanMailAppUITests : XCTestCase
@property (nonatomic, strong) XCUIApplication *app;
@property (nonatomic, strong) LNScreenshots *shots;
@end

@implementation LeanMailAppUITests

- (void)setUp {
    self.continueAfterFailure = NO;
    LNAppearTimeout = 30;
    LNEnabledTimeout = 30;
    self.shots = [LNScreenshots fromEnvironment:@"LEANMAIL_SCREENSHOTS"];
}

/// The mail row whose subject contains `text`, scrolled into view.
- (XCUIElement *)rowContaining:(NSString *)text {
    NSPredicate *p = [NSPredicate predicateWithFormat:@"identifier BEGINSWITH 'msg-' AND label CONTAINS[c] %@", text];
    XCUIElement *row = [[self.app.buttons matchingPredicate:p] firstMatch];
    LNPoll(30, 0.5, ^BOOL{ return row.exists || (LNScrollTo(self.app, row) && row.exists); });
    return row;
}

/// Launches with the test origin and signs in with the dev account; NO (the test skipped or failed) otherwise.
- (BOOL)signInAsDev {
    NSString *origin = NSProcessInfo.processInfo.environment[@"LEANMAIL_TEST_ORIGIN"];
    if (!origin.length) {
        XCTSkip(@"Set LEANMAIL_TEST_ORIGIN (a bare leanmail-server with DEV_AUTH_BYPASS=1) when generating the project");
        return NO;
    }
    self.app = [XCUIApplication new];
    self.app.launchEnvironment = @{@"LEANMAIL_TEST_ORIGIN": origin, @"LEANMAIL_RESET": @"1"};
    [self.app launch];
    if (!LNTap(self.app, @"dev-account") || !LNTap(self.app, @"dev-sign-in")) return NO;
    return [self.app.navigationBars[@"Inbox"] waitForExistenceWithTimeout:30];
}

/// Search, an attachment to the share sheet, the HTML part as a file, and a reply sent through /api/send
/// (a bare server has no Worker to deliver it: the answer is "not delivered", the copy kept in Sent).
- (void)testSearchAttachmentHtmlAndReply {
    if (![self signInAsDev]) return;
    XCUIElement *search = LNElement(self.app, @"search");
    if (!LNWait(self.app, @"search")) return;
    [search tap];
    [search typeText:@"invoice\n"];
    if (!LNWait(self.app, @"clear-search")) return;
    XCUIElement *row = [self rowContaining:@"Invoice 2026-0912"];
    XCTAssertTrue(row.exists);
    XCTAssertFalse([[self.app.buttons matchingPredicate:[NSPredicate predicateWithFormat:@"label CONTAINS 'Dinner Saturday?'"]] firstMatch].exists);
    LNShot(self, self.app, @"app-search", self.shots);
    if (!LNTap(self.app, row.identifier)) return;
    if (!LNTap(self.app, @"attachment-0")) return;
    if (!LNWait(self.app, @"share-attachment-0")) return;
    LNShot(self, self.app, @"app-attachment", self.shots);

    // a reply: the draft gets its client id, then Send
    if (!LNTap(self.app, @"reply")) return;
    if (!LNWait(self.app, @"compose-to")) return;
    XCTAssertTrue([LNElement(self.app, @"compose-to").value length] > 0, @"the reply has no recipient");
    // a bare server's dev account has no default address: name one (the dev principal may send as any)
    if (!LNType(self.app, @"compose-from", @"alex@wuli.nu")) return;
    if (!LNTap(self.app, @"compose-send")) return;
    if (!LNWait(self.app, @"compose-error")) return;
    XCTAssertTrue([LNText(LNElement(self.app, @"compose-error")) containsString:@"Not delivered"],
                  @"%@", LNText(LNElement(self.app, @"compose-error")));
    LNShot(self, self.app, @"app-compose", self.shots);
    if (!LNTap(self.app, @"compose-cancel")) return;

    // the HTML part of another message, written to a file for the share sheet
    [self.app.navigationBars.buttons.firstMatch tap];
    if (!LNTap(self.app, @"clear-search")) return;
    XCUIElement *logo = [self rowContaining:@"Logo options, round two"];
    if (!LNTap(self.app, logo.identifier)) return;
    if (!LNTap(self.app, @"open-html")) return;
    if (!LNWait(self.app, @"share-html")) return;
    LNShot(self, self.app, @"app-html", self.shots);
}

- (void)testDevSignInInboxOpenAndDone {
    NSString *origin = NSProcessInfo.processInfo.environment[@"LEANMAIL_TEST_ORIGIN"];
    if (!origin.length) {
        XCTSkip(@"Set LEANMAIL_TEST_ORIGIN (a bare leanmail-server with DEV_AUTH_BYPASS=1) when generating the project");
        return;
    }
    self.app = [XCUIApplication new];
    // a Debug build reads these: the origin prefilled, and any session a previous run saved forgotten
    self.app.launchEnvironment = @{@"LEANMAIL_TEST_ORIGIN": origin, @"LEANMAIL_RESET": @"1"};
    [self.app launch];

    if (!LNWait(self.app, @"origin")) return;
    XCTAssertEqualObjects(LNElement(self.app, @"origin").value, origin);
    LNShot(self, self.app, @"app-signin", self.shots);
    if (!LNTap(self.app, @"dev-account")) return;
    if (!LNTap(self.app, @"dev-sign-in")) return;

    // the inbox: the fixture mail, newest first
    NSString *subject = @"Proof pages for the autumn catalogue";
    XCTAssertTrue([self.app.navigationBars[@"Inbox"] waitForExistenceWithTimeout:30]);
    XCUIElement *row = [self rowContaining:subject];
    XCTAssertTrue(row.exists, @"no inbox row for %@", subject);
    XCTAssertTrue([[[self.app.buttons matchingPredicate:[NSPredicate predicateWithFormat:@"identifier BEGINSWITH 'msg-'"]] firstMatch] exists]);
    LNShot(self, self.app, @"app-inbox", self.shots);

    // open it: headers and the text body
    NSString *rowId = row.identifier;
    if (!LNTap(self.app, rowId)) return;
    if (!LNWait(self.app, @"message-body")) return;
    XCTAssertTrue(LNPoll(30, 0.2, ^BOOL{
        return [LNText(LNElement(self.app, @"message-body")) containsString:@"Three spreads still need the colour pass"]; }),
        @"body: %@", LNText(LNElement(self.app, @"message-body")));
    LNShot(self, self.app, @"app-message", self.shots);

    // mark it done: back to the inbox, the row gone, the notice shown
    if (!LNTap(self.app, @"action-done")) return;
    if (!LNWait(self.app, @"inbox-notice")) return;
    XCTAssertTrue([LNText(LNElement(self.app, @"inbox-notice")) containsString:subject]);
    XCTAssertTrue(LNExpectMissing(self.app, rowId, 3));
    LNShot(self, self.app, @"app-done", self.shots);
}

@end
