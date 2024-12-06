package studentConsulting.service.implement.common;

import com.google.api.client.util.DateTime;
import io.jsonwebtoken.Claims;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.*;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.request.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.*;
import studentConsulting.security.config.Email.EmailService;
import studentConsulting.security.jwt.JwtProvider;
import studentConsulting.service.interfaces.common.IStatusOnlineService;
import studentConsulting.service.interfaces.common.IUserService;
import studentConsulting.util.RandomUtils;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
public class UserServiceImpl implements IUserService {
    @Autowired
    RoleRepository roleRepository;
    @Autowired
    AccountRepository accountRepository;
    @Autowired
    AddressRepository addressRepository;
    @Autowired
    UserRepository userRepository;
    @Autowired
    PasswordEncoder passwordEncoder;
    /*
     * Đăng ký Tài Khoản
     */
    String urlConfirm;
    String body = "<!DOCTYPE html>\r\n"
            + "<html lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:o=\"urn:schemas-microsoft-com:office:office\">\r\n"
            + "<head>\r\n" + "    <meta charset=\"utf-8\"> <!-- utf-8 works for most cases -->\r\n"
            + "    <meta name=\"viewport\" content=\"width=device-width\"> <!-- Forcing initial-scale shouldn't be necessary -->\r\n"
            + "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"> <!-- Use the latest (edge) version of IE rendering engine -->\r\n"
            + "    <meta name=\"x-apple-disable-message-reformatting\">  <!-- Disable auto-scale in iOS 10 Mail entirely -->\r\n"
            + "    <title></title> <!-- The title tag shows in email notifications, like Android 4.4. -->\r\n" + "\r\n"
            + "    <link href=\"https://fonts.googleapis.com/css?family=Lato:300,400,700\" rel=\"stylesheet\">\r\n"
            + "\r\n" + "    <!-- CSS Reset : BEGIN -->\r\n" + "    <style>\r\n" + "\r\n"
            + "        /* What it does: Remove spaces around the email design added by some email clients. */\r\n"
            + "        /* Beware: It can remove the padding / margin and add a background color to the compose a reply window. */\r\n"
            + "        html,\r\n" + "body {\r\n" + "    margin: 0 auto !important;\r\n"
            + "    padding: 0 !important;\r\n" + "    height: 100% !important;\r\n" + "    width: 100% !important;\r\n"
            + "    background: #f1f1f1;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Stops email clients resizing small text. */\r\n" + "* {\r\n"
            + "    -ms-text-size-adjust: 100%;\r\n" + "    -webkit-text-size-adjust: 100%;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Centers email on Android 4.4 */\r\n" + "div[style*=\"margin: 16px 0\"] {\r\n"
            + "    margin: 0 !important;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Stops Outlook from adding extra spacing to tables. */\r\n" + "table,\r\n" + "td {\r\n"
            + "    mso-table-lspace: 0pt !important;\r\n" + "    mso-table-rspace: 0pt !important;\r\n" + "}\r\n"
            + "\r\n" + "/* What it does: Fixes webkit padding issue. */\r\n" + "table {\r\n"
            + "    border-spacing: 0 !important;\r\n" + "    border-collapse: collapse !important;\r\n"
            + "    table-layout: fixed !important;\r\n" + "    margin: 0 auto !important;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Uses a better rendering method when resizing images in IE. */\r\n" + "img {\r\n"
            + "    -ms-interpolation-mode:bicubic;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Prevents Windows 10 Mail from underlining links despite inline CSS. Styles for underlined links should be inline. */\r\n"
            + "a {\r\n" + "    text-decoration: none;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: A work-around for email clients meddling in triggered links. */\r\n"
            + "*[x-apple-data-detectors],  /* iOS */\r\n" + ".unstyle-auto-detected-links *,\r\n" + ".aBn {\r\n"
            + "    border-bottom: 0 !important;\r\n" + "    cursor: default !important;\r\n"
            + "    color: inherit !important;\r\n" + "    text-decoration: none !important;\r\n"
            + "    font-size: inherit !important;\r\n" + "    font-family: inherit !important;\r\n"
            + "    font-weight: inherit !important;\r\n" + "    line-height: inherit !important;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Prevents Gmail from displaying a download button on large, non-linked images. */\r\n"
            + ".a6S {\r\n" + "    display: none !important;\r\n" + "    opacity: 0.01 !important;\r\n" + "}\r\n"
            + "\r\n" + "/* What it does: Prevents Gmail from changing the text color in conversation threads. */\r\n"
            + ".im {\r\n" + "    color: inherit !important;\r\n" + "}\r\n" + "\r\n"
            + "/* If the above doesn't work, add a .g-img class to any image in question. */\r\n"
            + "img.g-img + div {\r\n" + "    display: none !important;\r\n" + "}\r\n" + "\r\n"
            + "/* What it does: Removes right gutter in Gmail iOS app: https://github.com/TedGoas/Cerberus/issues/89  */\r\n"
            + "/* Create one of these media queries for each additional viewport size you'd like to fix */\r\n" + "\r\n"
            + "/* iPhone 4, 4S, 5, 5S, 5C, and 5SE */\r\n"
            + "@media only screen and (min-device-width: 320px) and (max-device-width: 374px) {\r\n"
            + "    u ~ div .email-container {\r\n" + "        min-width: 320px !important;\r\n" + "    }\r\n" + "}\r\n"
            + "/* iPhone 6, 6S, 7, 8, and X */\r\n"
            + "@media only screen and (min-device-width: 375px) and (max-device-width: 413px) {\r\n"
            + "    u ~ div .email-container {\r\n" + "        min-width: 375px !important;\r\n" + "    }\r\n" + "}\r\n"
            + "/* iPhone 6+, 7+, and 8+ */\r\n" + "@media only screen and (min-device-width: 414px) {\r\n"
            + "    u ~ div .email-container {\r\n" + "        min-width: 414px !important;\r\n" + "    }\r\n" + "}\r\n"
            + "\r\n" + "    </style>\r\n" + "\r\n" + "    <!-- CSS Reset : END -->\r\n" + "\r\n"
            + "    <!-- Progressive Enhancements : BEGIN -->\r\n" + "    <style>\r\n" + "\r\n" + "	    .primary{\r\n"
            + "	background: #30e3ca;\r\n" + "}\r\n" + ".bg_white{\r\n" + "	background: #ffffff;\r\n" + "}\r\n"
            + ".bg_light{\r\n" + "	background: #fafafa;\r\n" + "}\r\n" + ".bg_black{\r\n"
            + "	background: #000000;\r\n" + "}\r\n" + ".bg_dark{\r\n" + "	background: rgba(0,0,0,.8);\r\n" + "}\r\n"
            + ".email-section{\r\n" + "	padding:2.5em;\r\n" + "}\r\n" + "\r\n" + "/*BUTTON*/\r\n" + ".btn{\r\n"
            + "	padding: 10px 15px;\r\n" + "	display: inline-block;\r\n" + "}\r\n" + ".btn.btn-primary{\r\n"
            + "	border-radius: 5px;\r\n" + "	background: #30e3ca;\r\n" + "	color: #ffffff;\r\n" + "}\r\n"
            + ".btn.btn-white{\r\n" + "	border-radius: 5px;\r\n" + "	background: #ffffff;\r\n"
            + "	color: #000000;\r\n" + "}\r\n" + ".btn.btn-white-outline{\r\n" + "	border-radius: 5px;\r\n"
            + "	background: transparent;\r\n" + "	border: 1px solid #fff;\r\n" + "	color: #fff;\r\n" + "}\r\n"
            + ".btn.btn-black-outline{\r\n" + "	border-radius: 0px;\r\n" + "	background: transparent;\r\n"
            + "	border: 2px solid #000;\r\n" + "	color: #000;\r\n" + "	font-weight: 700;\r\n" + "}\r\n" + "\r\n"
            + "h1,h2,h3,h4,h5,h6{\r\n" + "	font-family: 'Times New Roman', Times, serif;\r\n" + "	color: #000000;\r\n"
            + "	margin-top: 0;\r\n" + "	font-weight: 400;\r\n" + "}\r\n" + "\r\n" + "body{\r\n"
            + "	font-family: 'Lato', sans-serif;\r\n" + "	font-weight: 400;\r\n" + "	font-size: 15px;\r\n"
            + "	line-height: 1.8;\r\n" + "	color: rgba(0,0,0,.4);\r\n" + "}\r\n" + "\r\n" + "a{\r\n"
            + "	color: #30e3ca;\r\n" + "}\r\n" + "\r\n" + "table{\r\n" + "}\r\n" + "/*LOGO*/\r\n" + "\r\n"
            + ".logo h1{\r\n" + "	margin: 0;\r\n" + "}\r\n" + ".logo h1 a{\r\n" + "	color: #30e3ca;\r\n"
            + "	font-size: 24px;\r\n" + "	font-weight: 700;\r\n" + "	font-family: 'Lato', sans-serif;\r\n" + "}\r\n"
            + "\r\n" + "/*HERO*/\r\n" + ".hero{\r\n" + "	position: relative;\r\n" + "	z-index: 0;\r\n" + "}\r\n"
            + "\r\n" + ".hero .text{\r\n" + "	color: rgba(0,0,0,.3);\r\n" + "}\r\n" + ".hero .text h2{\r\n"
            + "	color: #000;\r\n" + "	font-size: 40px;\r\n" + "	margin-bottom: 0;\r\n" + "	font-weight: 400;\r\n"
            + "	line-height: 1.4;\r\n" + "}\r\n" + ".hero .text h3{\r\n" + "	font-size: 24px;\r\n"
            + "	font-weight: 300;\r\n" + "}\r\n" + ".hero .text h2 span{\r\n" + "	font-weight: 600;\r\n"
            + "	color: #30e3ca;\r\n" + "}\r\n" + "\r\n" + "\r\n" + "/*HEADING SECTION*/\r\n" + ".heading-section{\r\n"
            + "}\r\n" + ".heading-section h2{\r\n" + "	color: #000000;\r\n" + "	font-size: 28px;\r\n"
            + "	margin-top: 0;\r\n" + "	line-height: 1.4;\r\n" + "	font-weight: 400;\r\n" + "}\r\n"
            + ".heading-section .subheading{\r\n" + "	margin-bottom: 20px !important;\r\n"
            + "	display: inline-block;\r\n" + "	font-size: 13px;\r\n" + "	text-transform: uppercase;\r\n"
            + "	letter-spacing: 2px;\r\n" + "	color: rgba(0,0,0,.4);\r\n" + "	position: relative;\r\n" + "}\r\n"
            + ".heading-section .subheading::after{\r\n" + "	position: absolute;\r\n" + "	left: 0;\r\n"
            + "	right: 0;\r\n" + "	bottom: -10px;\r\n" + "	content: '';\r\n" + "	width: 100%;\r\n"
            + "	height: 2px;\r\n" + "	background: #30e3ca;\r\n" + "	margin: 0 auto;\r\n" + "}\r\n" + "\r\n"
            + ".heading-section-white{\r\n" + "	color: rgba(255,255,255,.8);\r\n" + "}\r\n"
            + ".heading-section-white h2{\r\n" + "	/* font-family:  */\r\n" + "	line-height: 1;\r\n"
            + "	padding-bottom: 0;\r\n" + "}\r\n" + ".heading-section-white h2{\r\n" + "	color: #ffffff;\r\n"
            + "}\r\n" + ".heading-section-white .subheading{\r\n" + "	margin-bottom: 0;\r\n"
            + "	display: inline-block;\r\n" + "	font-size: 13px;\r\n" + "	text-transform: uppercase;\r\n"
            + "	letter-spacing: 2px;\r\n" + "	color: rgba(255,255,255,.4);\r\n" + "}\r\n" + "\r\n" + "\r\n"
            + "ul.social{\r\n" + "	padding: 0;\r\n" + "}\r\n" + "ul.social li{\r\n" + "	display: inline-block;\r\n"
            + "	margin-right: 10px;\r\n" + "}\r\n" + "\r\n" + "/*FOOTER*/\r\n" + "\r\n" + ".footer{\r\n"
            + "	border-top: 1px solid rgba(0,0,0,.05);\r\n" + "	color: rgba(0,0,0,.5);\r\n" + "}\r\n"
            + ".footer .heading{\r\n" + "	color: #000;\r\n" + "	font-size: 20px;\r\n" + "}\r\n" + ".footer ul{\r\n"
            + "	margin: 0;\r\n" + "	padding: 0;\r\n" + "}\r\n" + ".footer ul li{\r\n" + "	list-style: none;\r\n"
            + "	margin-bottom: 10px;\r\n" + "}\r\n" + ".footer ul li a{\r\n" + "	color: rgba(0,0,0,1);\r\n" + "}\r\n"
            + "\r\n" + "\r\n" + "@media screen and (max-width: 500px) {\r\n" + "\r\n" + "\r\n" + "}\r\n" + "\r\n"
            + "\r\n" + "    </style>\r\n" + "\r\n" + "\r\n" + "</head>\r\n" + "\r\n"
            + "<body width=\"100%\" style=\"margin: 0; padding: 0 !important; mso-line-height-rule: exactly; background-color: #f1f1f1;\">\r\n"
            + "	<center style=\"width: 100%; background-color: #f1f1f1;\">\r\n"
            + "    <div style=\"display: none; font-size: 1px;max-height: 0px; max-width: 0px; opacity: 0; overflow: hidden; mso-hide: all; font-family: sans-serif;\">\r\n"
            + "      &zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;&zwnj;&nbsp;\r\n"
            + "    </div>\r\n" + "    <div style=\"max-width: 600px; margin: 0 auto;\" class=\"email-container\">\r\n"
            + "    	<!-- BEGIN BODY -->\r\n"
            + "      <table align=\"center\" role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\" style=\"margin: auto;\">\r\n"
            + "      	<tr>\r\n"
            + "          <td valign=\"top\" class=\"bg_white\" style=\"padding: 1em 2.5em 0 2.5em;\">\r\n"
            + "          	<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\r\n"
            + "          		<tr>\r\n" + "          			<td class=\"logo\" style=\"text-align: center;\">\r\n"
            + "			            <h1><a href=\"#\">Email</a></h1>\r\n" + "			          </td>\r\n"
            + "          		</tr>\r\n" + "          	</table>\r\n" + "          </td>\r\n"
            + "	      </tr><!-- end tr -->\r\n" + "	      <tr>\r\n"
            + "          <td valign=\"middle\" class=\"hero bg_white\" style=\"padding: 3em 0 2em 0;\">\r\n"
            + "            <img src=\"https://firebasestorage.googleapis.com/v0/b/davitickets-2e627.appspot.com/o/email.png?alt=media&token=6f31d4c1-1a7d-4fb5-bf90-f836224f0ea6\" alt=\"\" style=\"width: 300px; max-width: 600px; height: auto; margin: auto; display: block;\">\r\n"
            + "          </td>\r\n" + "	      </tr><!-- end tr -->\r\n" + "				<tr>\r\n"
            + "          <td valign=\"middle\" class=\"hero bg_white\" style=\"padding: 2em 0 4em 0;\">\r\n"
            + "            <table>\r\n" + "            	<tr>\r\n" + "            		<td>\r\n";
    String footer = "</td>\r\n" + "            	</tr>\r\n" + "            </table>\r\n" + "          </td>\r\n"
            + "	      </tr><!-- end tr -->\r\n" + "      <!-- 1 Column Text + Button : END -->\r\n"
            + "      </table>\r\n"
            + "      <table align=\"center\" role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\" style=\"margin: auto;\">\r\n"
            + "      	<tr>\r\n" + "          <td valign=\"middle\" class=\"bg_light footer email-section\">\r\n"
            + "            <table>\r\n" + "            	<tr>\r\n"
            + "                <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
            + "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
            + "                    <tr>\r\n"
            + "                      <td style=\"text-align: left; padding-right: 10px;\">\r\n"
            + "                      	<h3 class=\"heading\">Về chúng tôi</h3>\r\n"
            + "                      	<p>Website tư vấn sinh viên hcmute\r\n" + "                            </p>\r\n"
            + "                      </td>\r\n" + "                    </tr>\r\n" + "                  </table>\r\n"
            + "                </td>\r\n"
            + "                <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
            + "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
            + "                    <tr>\r\n"
            + "                      <td style=\"text-align: left; padding-left: 5px; padding-right: 5px;\">\r\n"
            + "                      	<h3 class=\"heading\">Liên hệ</h3>\r\n" + "                      	<ul>\r\n"
            + "					                <li><span class=\"text\">HCMUTE</span></li>\r\n"
            + "					                <li><span class=\"text\">0974117373</span></a></li>\r\n"
            + "					              </ul>\r\n" + "                      </td>\r\n"
            + "                    </tr>\r\n" + "                  </table>\r\n" + "                </td>\r\n"
            + "                <!-- <td valign=\"top\" width=\"33.333%\" style=\"padding-top: 20px;\">\r\n"
            + "                  <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" width=\"100%\">\r\n"
            + "                    <tr>\r\n"
            + "                      <td style=\"text-align: left; padding-left: 10px;\">\r\n"
            + "                      	<h3 class=\"heading\">Useful Links</h3>\r\n"
            + "                      	<ul>\r\n"
            + "					                <li><a href=\"#\">Home</a></li>\r\n"
            + "					                <li><a href=\"#\">About</a></li>\r\n"
            + "					                <li><a href=\"#\">Services</a></li>\r\n"
            + "					                <li><a href=\"#\">Work</a></li>\r\n"
            + "					              </ul>\r\n" + "                      </td>\r\n"
            + "                    </tr>\r\n" + "                  </table>\r\n" + "                </td>\r\n"
            + "              </tr>\r\n" + "            </table>\r\n" + "          </td> -->\r\n"
            + "        </tr><!-- end: tr -->\r\n" + "        <!-- <tr>\r\n"
            + "          <td class=\"bg_light\" style=\"text-align: center;\">\r\n"
            + "          	<p>No longer want to receive these email? You can <a href=\"#\" style=\"color: rgba(0,0,0,.8);\">Unsubscribe here</a></p>\r\n"
            + "          </td>\r\n" + "        </tr> -->\r\n" + "      </table>\r\n" + "\r\n" + "    </div>\r\n"
            + "  </center>\r\n" + "</body>\r\n" + "</html>";
    @Autowired
    private ProvinceRepository provinceRepository;
    @Autowired
    private DistrictRepository districtRepository;
    @Autowired
    private WardRepository wardRepository;
    @Autowired
    private IStatusOnlineService commonStatusOnlineServiceImpl;
    @Autowired
    private JwtProvider jwtProvider;
    @Autowired
    private RoleAuthRepository tokenRepository;
    @Autowired
    private JavaMailSender javaMailSender;
    @Autowired
    private EmailService emailService;
   

    public Integer getUserIdFromToken(String token) {
        Claims claims = jwtProvider.getClaimsFromToken(token);
        return Integer.parseInt(claims.getSubject());
    }

    // build token
    // Tạo ra và lưu trữ một token mới, sau đó trả về thông tin phản hồi đăng nhập bao gồm token truy cập,
    // thời gian hết hạn, và mã định danh token để người dùng có thể sử dụng trong các yêu cầu tiếp theo.
    /*
     * Xây dựng Token
     */
    private DataResponse<DataResponse.LoginData> buildToken(UserInformationEntity userModel) {
        try {
            String jti = UUID.randomUUID().toString();
            long expiredTime = System.currentTimeMillis() + jwtProvider.getRefreshTokenExpirationMs();

            // Lưu refreshToken vào cơ sở dữ liệu
            tokenRepository.save(RoleAuthEntity.builder()
                    .user(userModel)
                    .tokenId(jti)
                    .expiredTime(expiredTime)
                    .build());

            // Tạo access token
            String accessToken = jwtProvider.createToken(userModel);

            // Tạo đối tượng UserInformationDTO
            UserInformationDTO userDto = UserInformationDTO.builder()
                    .id(userModel.getId())
                    .schoolName(userModel.getSchoolName())
                    .firstName(userModel.getFirstName())
                    .lastName(userModel.getLastName())
                    .phone(userModel.getPhone())
                    .avatarUrl(userModel.getAvatarUrl())
                    .gender(userModel.getGender())
                    .build();

            // Xây dựng phản hồi DataResponse với LoginData
            DataResponse.LoginData loginData = DataResponse.LoginData.builder()
                    .user(userDto)
                    .accessToken(accessToken)
                    .expiresIn(System.currentTimeMillis() + jwtProvider.getJwtExpirationMs()) // Hạn của access token
                    .refreshToken(jti)  // Refresh token
                    .build();

            return DataResponse.<DataResponse.LoginData>builder()
                    .status("success")
                    .message("Login successful")
                    .data(loginData)
                    .build();
        } catch (Exception e) {
            System.err.println("Error building token: " + e.getMessage());
            e.printStackTrace();
            throw new RuntimeException("Error building token", e);
        }
    }


    /*
     * Xử lý Refresh Token
     */
    public DataResponse<DataResponse.LoginData> refreshToken(String refreshToken) {
        System.out.println("Received refresh token: " + refreshToken);

        RoleAuthEntity tokenModel = getValidToken(refreshToken);

        Optional<UserInformationEntity> userModelOptional = userRepository.findById(tokenModel.getUser().getId());

        if (userModelOptional.isPresent()) {
            UserInformationEntity userModel = userModelOptional.get();

            String email = userModel.getAccount().getEmail();  // Lấy email từ tài khoản của người dùng
            commonStatusOnlineServiceImpl.updateStatus(email, true);

            // Tạo mới access token
            String newAccessToken = jwtProvider.createToken(userModel);
            System.out.println("New Access Token: " + newAccessToken);

            DataResponse.LoginData loginData = DataResponse.LoginData.builder()
                    .user(UserInformationDTO.builder()
                            .id(userModel.getId())
                            .schoolName(userModel.getSchoolName())
                            .firstName(userModel.getFirstName())
                            .lastName(userModel.getLastName())
                            .phone(userModel.getPhone())
                            .avatarUrl(userModel.getAvatarUrl())
                            .gender(userModel.getGender())
                            .build())
                    .accessToken(newAccessToken)
                    .expiresIn(System.currentTimeMillis() + jwtProvider.getJwtExpirationMs())
                    .refreshToken(refreshToken)
                    .build();

            return DataResponse.<DataResponse.LoginData>builder()
                    .status("success")
                    .message("Token refreshed successfully")
                    .data(loginData)
                    .build();
        } else {
            throw new ErrorException("User not found!");
        }
    }


    private RoleAuthEntity getValidToken(String refreshToken) {
        RoleAuthEntity tokenModel = tokenRepository.findByTokenId(refreshToken);

        if (tokenModel == null || tokenModel.getId() <= 0) {
            throw new ErrorException("Mã refresh token không tồn tại");
        } else if (System.currentTimeMillis() > tokenModel.getExpiredTime()) {
            throw new ErrorException("Mã refresh token đã hết hạn lúc " + new DateTime(tokenModel.getExpiredTime()));
        } else if (System.currentTimeMillis() > tokenModel.getExpiredTime() + jwtProvider.getRefreshTokenExpirationMs()) {
            throw new ErrorException("Mã refresh token đã quá thời hạn sử dụng.");
        }

        return tokenModel;
    }


    @Override
    public DataResponse<UserInformationDTO> register(RegisterRequest registerRequest) {
        validateRegistrationFields(registerRequest);
        checkAccountExistence(registerRequest);
        String verifyTokens = RandomUtils.getRandomVerifyCode();
        urlConfirm = verifyTokens;

        AccountEntity accountModel = createAccount(registerRequest, verifyTokens);
        accountRepository.save(accountModel);

        UserInformationEntity userModel = createUser(registerRequest, accountModel);
        userModel.setAvatarUrl(FilePaths.AVATAR_URL);
        userRepository.save(userModel);

        sendRegistrationEmail(registerRequest.getEmail(), verifyTokens, accountModel);

        AccountDTO accountDto = AccountDTO.builder()
                .id(accountModel.getId())
                .username(accountModel.getUsername())
                .email(accountModel.getEmail())
                .isActivity(accountModel.isActivity())
                .verifyRegister(accountModel.getVerifyRegister())
                .build();

        UserInformationDTO userDto = UserInformationDTO.builder()
                .id(userModel.getId())
                .schoolName(userModel.getSchoolName())
                .firstName(userModel.getFirstName())
                .lastName(userModel.getLastName())
                .phone(userModel.getPhone())
                .avatarUrl(userModel.getAvatarUrl())
                .gender(userModel.getGender())
                .account(accountDto)
                .build();

        return DataResponse.<UserInformationDTO>builder()
                .status("success")
                .message("Đăng ký thành công! Vui lòng kiểm tra email để xác nhận đăng ký.")
                .data(userDto)
                .build();
    }

    private void checkAccountExistence(RegisterRequest registerRequest) {
        AccountEntity existingAccount = accountRepository.findAccountByUsername(registerRequest.getUsername());
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (existingAccount != null && existingAccount.getId() >= 0) {
            throw new ErrorException("Tài khoản đã tồn tại. Vui lòng nhập lại!");
        }

        if (accountRepository.existsByEmail(registerRequest.getEmail())) {
            errors.add(new FieldErrorDetail("email", "Email đã tồn tại. Vui lòng nhập lại!"));
        }

        if (userRepository.existsByPhone(registerRequest.getPhone())) {
            errors.add(new FieldErrorDetail("phone", "Số điện thoại đã tồn tại. Vui lòng nhập lại!"));
        }
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
    }

    private AccountEntity createAccount(RegisterRequest registerRequest, String verifyTokens) {
        RoleEntity roleModel = roleRepository.findByName(SecurityConstants.Role.USER);
        if (roleModel == null) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        String hashedPassword = passwordEncoder.encode(registerRequest.getPassword());
        LocalDateTime expirationTime = LocalDateTime.now().plusMinutes(5);

        return AccountEntity.builder()
                .username(registerRequest.getUsername())
                .role(roleModel)
                .email(registerRequest.getEmail())
                .password(hashedPassword)
                .isActivity(false)
                .verifyRegister(verifyTokens)
                .createdAt(LocalDate.now())
                .verifyCodeExpirationTime(expirationTime)
                .build();
    }

    private UserInformationEntity createUser(RegisterRequest registerRequest, AccountEntity accountModel) {
        return UserInformationEntity.builder()
                .phone(registerRequest.getPhone())
                .gender(registerRequest.getGender())
                .account(accountModel)
                .createdAt(LocalDate.now())
                .build();
    }

    private void validateRegistrationFields(RegisterRequest registerRequest) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (!isValidEmail(registerRequest.getEmail())) {
            errors.add(new FieldErrorDetail("email", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        if (!isStrongPassword(registerRequest.getPassword())) {
            errors.add(new FieldErrorDetail("password", "Mật khẩu phải chứa ít nhất 12 ký tự, bao gồm chữ hoa, chữ thường, số và ký tự đặc biệt."));
        }

        if (!isValidPhoneNumber(registerRequest.getPhone())) {
            errors.add(new FieldErrorDetail("phone", "Số điện thoại không hợp lệ! Số điện thoại chỉ có 10 số và không chứa ký tự chữ cái."));
        }

        if (!registerRequest.getPassword().equals(registerRequest.getConfirmPassword())) {
            errors.add(new FieldErrorDetail("password", "Mật khẩu và xác nhận mật khẩu không khớp."));
        }
//        if (!isValidGender(registerRequest.getGender())) {
//            errors.add(new FieldErrorDetail("gender", "Giới tính không hợp lệ! Chỉ chấp nhận giá trị 'NAM' hoặc 'NU'."));
//        } else if (registerRequest.getGender().length() > 3) {
//            errors.add(new FieldErrorDetail("gender", "Giới tính không được vượt quá 3 ký tự."));
//        }
        if (!registerRequest.getUsername().matches("^[a-zA-Z]+$")) {
            errors.add(new FieldErrorDetail("username", "Tên người dùng chỉ được chứa các chữ cái."));
        }
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
    }

    private boolean isValidGender(String gender) {
        return "NAM".equalsIgnoreCase(gender) || "NU".equalsIgnoreCase(gender);
    }

    private boolean isStrongPassword(String password) {
        String passwordRegex = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&#])[A-Za-z\\d@$!%*?&#]{12,}$";
        Pattern pattern = Pattern.compile(passwordRegex);
        return pattern.matcher(password).matches();
    }

    private boolean isValidPhoneNumber(String phoneNumber) {
        String phoneRegex = "^[0-9]{10}$";
        Pattern pattern = Pattern.compile(phoneRegex);
        return pattern.matcher(phoneNumber).matches();
    }

    private void sendRegistrationEmail(String email, String verifyTokens, AccountEntity account) {
        try {
            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(email);
            mailHelper.setSubject("Xác nhận đăng ký tài khoản");
            mailHelper.setText(
                    body + "<div class=\"text\" style=\"padding: 0 2.5em; text-align: center;\">\r\n"
                            + "    <h3>Cảm ơn bạn đã đăng ký tài khoản\r\n"
                            + "</h3>\r\n"
                            + "    <h4>Vui lòng nhấp vào liên kết dưới đây để xác nhận đăng ký tài khoản:\r\n"
                            + "</h4>\r\n"
                            + "    <p>" + verifyTokens + "</p>\r\n"
                            + "</div>\r\n"
                            + footer, true);
            javaMailSender.send(mailMessage);

            account.setVerifyRegister(verifyTokens);
            account.setVerifyCodeExpirationTime(LocalDateTime.now().plusMinutes(5)); // 5 phút
            account.setVerifyCodeAttemptCount(0);
            accountRepository.save(account);
        } catch (MessagingException e) {
            throw new ErrorException("Lỗi gửi email xác nhận");
        }
    }

    /*
     * Xác Nhận Đăng Ký
     */
    @Override
    public DataResponse<Object> confirmRegistration(ConfirmRegistrationRequest confirmRegistrationRequest) {
        AccountEntity account = accountRepository.findAccountByEmail(confirmRegistrationRequest.getEmailRequest());
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (account == null || account.getId() <= 0) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        // Kiểm tra mã xác nhận đã hết hạn chưa
        if (LocalDateTime.now().isAfter(account.getVerifyCodeExpirationTime())) {
            errors.add(new FieldErrorDetail("token", "Mã xác nhận đã hết hạn!"));
        }

        // Kiểm tra số lần nhập mã
        if (account.getVerifyCodeAttemptCount() >= 3) {
            errors.add(new FieldErrorDetail("token", "Bạn đã nhập sai mã xác nhận quá số lần cho phép. Mã xác nhận đã bị vô hiệu."));
        }

        // Kiểm tra mã xác nhận
        if (!account.getVerifyRegister().equals(confirmRegistrationRequest.getToken())) {
            account.setVerifyCodeAttemptCount(account.getVerifyCodeAttemptCount() + 1);
            accountRepository.save(account);
            errors.add(new FieldErrorDetail("token", "Mã xác thực không đúng. Vui lòng kiểm tra lại!"));
        }

        // Mã xác nhận đúng, kích hoạt tài khoản và reset số lần nhập
        account.setActivity(true);
        account.setVerifyRegister(null);
        account.setVerifyCodeAttemptCount(0);
        account.setVerifyCodeExpirationTime(null);
        accountRepository.save(account);
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
        return DataResponse.builder().status("success").message("Xác nhận thành công!").build();
    }

    /*
     * Đăng Nhập
     */
    private List<FieldErrorDetail> validateLoginFields(LoginRequest loginRequest, AccountEntity accountModel) {
        List<FieldErrorDetail> errors = new ArrayList<>();
        if (!isValidEmail(loginRequest.getEmail())) {
            errors.add(new FieldErrorDetail("email", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        if (accountModel == null) {
            errors.add(new FieldErrorDetail("email", "Email không tồn tại trong hệ thống."));
        } else {
            if (!accountModel.isActivity()) {
                throw new ErrorException("Tài khoản đã bị khóa! Vui lòng liên hệ với quản trị viên.");
            }
            if (!passwordEncoder.matches(loginRequest.getPassword(), accountModel.getPassword())) {
                errors.add(new FieldErrorDetail("password", "Mật khẩu không chính xác! Vui lòng thử lại."));
            }
        }

        return errors;
    }

    @Override
    public DataResponse<DataResponse.LoginData> login(LoginRequest loginRequest) {
        AccountEntity accountModel = accountRepository.findAccountByEmail(loginRequest.getEmail());

        List<FieldErrorDetail> errors = validateLoginFields(loginRequest, accountModel);

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        return buildToken(userRepository.findUserInfoModelByAccountModel(accountModel));
    }

    private boolean isValidEmail(String email) {
        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\.[a-zA-Z0-9_+&*-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,7}$";
        Pattern pattern = Pattern.compile(emailRegex);
        if (email == null) {
            return false;
        }
        return pattern.matcher(email).matches();
    }

    /*
     * Thay Đổi Mật Khẩu
     */
    @Transactional
    @Override
    public DataResponse<Object> changePassword(String email, ChangePasswordRequest changePasswordRequest) {
        AccountEntity account = accountRepository.findAccountByEmail(email);
        if (account == null) {
            throw new ErrorException("Tài khoản không tồn tại");
        }

        List<FieldErrorDetail> errors = new ArrayList<>();

        if (!passwordEncoder.matches(changePasswordRequest.getCurrentPassword(), account.getPassword())) {
            errors.add(new FieldErrorDetail("currentPassword", "Nhập sai mật khẩu hiện tại"));
        }

        if (!changePasswordRequest.getNewPassword().equals(changePasswordRequest.getConfirmNewPassword())) {
            errors.add(new FieldErrorDetail("confirmNewPassword", "Xác nhận mật khẩu mới không khớp"));
        }

        if (passwordEncoder.matches(changePasswordRequest.getNewPassword(), account.getPassword())) {
            errors.add(new FieldErrorDetail("newPassword", "Mật khẩu mới không được trùng với mật khẩu hiện tại"));
        }

        if (!isStrongPassword(changePasswordRequest.getNewPassword())) {
            errors.add(new FieldErrorDetail("newPassword", "Mật khẩu mới phải chứa ít nhất 12 ký tự, bao gồm chữ hoa, chữ thường, số và ký tự đặc biệt"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        String hashedPassword = passwordEncoder.encode(changePasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        accountRepository.save(account);

        return DataResponse.builder()
                .status("success")
                .message("Thay đổi mật khẩu thành công")
                .build();
    }

    /*
     * Quên Mật Khẩu
     */
    @Override
    public DataResponse<Object> forgotPassword(ForgotPasswordRequest forgotPasswordRequest) {
        List<FieldErrorDetail> errors = new ArrayList<>();


        if (!isValidEmail(forgotPasswordRequest.getEmailRequest())) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        AccountEntity account = accountRepository.findAccountByEmail(forgotPasswordRequest.getEmailRequest());
        if (account == null) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không tồn tại trong hệ thống. Vui lòng kiểm tra lại."));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        // Kiểm tra xem tài khoản có đang hoạt động không
        if (!account.isActivity()) {
            throw new ErrorException("Tài khoản đã bị khóa! Không thể yêu cầu đặt lại mật khẩu.");
        }

        // Tạo mã xác nhận mới và gửi email
        String verifyCode = RandomUtils.getRandomVerifyCode();
        account.setVerifyCodeExpirationTime(LocalDateTime.now().plusMinutes(5)); // 5 phút
        sendForgotPasswordEmail(forgotPasswordRequest.getEmailRequest(), verifyCode, account);

        return DataResponse.builder()
                .status("success")
                .message("Mã xác nhận đã được gửi qua email!")
                .build();
    }

    private void sendForgotPasswordEmail(String email, String verifyCode, AccountEntity account) {
        try {
            MimeMessage mailMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper mailHelper = new MimeMessageHelper(mailMessage, true, "UTF-8");
            mailHelper.setFrom("ngoquangnghia111003@gmail.com");
            mailHelper.setTo(email);
            mailHelper.setSubject("Mã xác nhận lấy lại mật khẩu");
            mailHelper.setText(
                    body + "<div class=\"text\" style=\"padding: 0 2.5em; text-align: center;\">\r\n"
                            + "    <h3>Bạn vừa yêu cầu cập nhật lại mật khẩu</h3>\r\n"
                            + "    <h4>Đây là mã xác nhận lấy lại mật khẩu của bạn</h4>\r\n"
                            + "    <p>" + verifyCode + "</p>\r\n"
                            + "</div>\r\n"
                            + footer, true);
            emailService.sendEmail(mailMessage);

            // Cập nhật thời gian hết hạn và số lần gửi mã xác nhận
            account.setVerifyCode(verifyCode);
            account.setVerifyCodeExpirationTime(LocalDateTime.now().plusMinutes(5)); // 5 phút
            account.setVerifyCodeAttemptCount(0);
            accountRepository.save(account);
        } catch (Exception e) {
            throw new ErrorException("Lỗi gửi mã xác nhận!");
        }
    }

    /*
     * Kiểm Tra Mã Xác Nhận
     */
    @Override
    public DataResponse<Object> checkVerifyCode(VerifyCodeCheckRequest verifyCode) {
        AccountEntity account = accountRepository.findAccountByEmail(verifyCode.getEmailRequest());
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (account == null || account.getId() <= 0) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        if (account != null && LocalDateTime.now().isAfter(account.getVerifyCodeExpirationTime())) {
            errors.add(new FieldErrorDetail("code", "Mã xác nhận đã hết hạn!"));
        }

        if (account != null && account.getVerifyCodeAttemptCount() >= 3) {
            errors.add(new FieldErrorDetail("code", "Bạn đã nhập sai mã xác nhận quá số lần cho phép. Mã xác nhận đã bị vô hiệu."));
        }

        if (account != null && !account.getVerifyCode().equals(verifyCode.getCode())) {
            account.setVerifyCodeAttemptCount(account.getVerifyCodeAttemptCount() + 1);
            accountRepository.save(account);
            errors.add(new FieldErrorDetail("code", "Sai mã xác thực!"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        accountRepository.save(account);

        return DataResponse.builder().status("success").message("Xác thực mã thành công!").build();
    }

    /*
     * Đặt Lại Mật Khẩu
     */
    @Override
    public DataResponse<Object> resetPassword(ResetPasswordRequest resetPasswordRequest) {
        AccountEntity account = accountRepository.findAccountByEmail(resetPasswordRequest.getEmail());
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (account == null || account.getId() <= 0) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        if (!account.isActivity()) {
            throw new ErrorException("Tài khoản đã bị khóa! Vui lòng liên hệ với quản trị viên.");
        }

        if (!isStrongPassword(resetPasswordRequest.getNewPassword())) {
            errors.add(new FieldErrorDetail("newPassword", "Mật khẩu mới không đáp ứng các yêu cầu bảo mật (phải có chữ hoa, thường, số, ký tự đặc biệt và trên 12 ký tự)."));
        }

        if (!resetPasswordRequest.getNewPassword().equals(resetPasswordRequest.getRepeatPassword())) {
            errors.add(new FieldErrorDetail("repeatPassword", "Mật khẩu mới và xác nhận mật khẩu không khớp."));
        }

        if(!resetPasswordRequest.getToken().equals(account.getVerifyCode())){
            throw new ErrorException("Token không khớp");
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        String hashedPassword = passwordEncoder.encode(resetPasswordRequest.getNewPassword());
        account.setPassword(hashedPassword);
        account.setVerifyCode(null);
        account.setVerifyCodeAttemptCount(0);
        account.setVerifyCodeExpirationTime(null);
        accountRepository.save(account);

        return DataResponse.builder().status("success").message("Cập nhật mật khẩu thành công!").build();
    }

    /*
     * Gửi lại mã xác nhận khi quá hạn hoặc quá số lần
     */
    @Override
    public DataResponse<Object> resendVerificationCodeForRegister(ResendVerificationRequest resendRequest) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        if (!isValidEmail(resendRequest.getEmailRequest())) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        AccountEntity account = accountRepository.findAccountByEmail(resendRequest.getEmailRequest());
        if (account == null) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không tồn tại trong hệ thống."));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        // Kiểm tra nếu mã đã bị vô hiệu hóa do nhập sai quá nhiều lần hoặc đã hết hạn
        boolean isCodeExpired = LocalDateTime.now().isAfter(account.getVerifyCodeExpirationTime());

        if (account.getVerifyCodeAttemptCount() >= 3 || isCodeExpired) {
            String newVerifyCode = RandomUtils.getRandomVerifyCode();
            sendRegistrationEmail(account.getEmail(), newVerifyCode, account); // Gửi mã xác nhận mới cho đăng ký

            // Cập nhật lại thông tin mã xác nhận mới
            account.setVerifyRegister(newVerifyCode);
            account.setVerifyCodeExpirationTime(LocalDateTime.now().plusMinutes(5)); // 5 phút
            account.setVerifyCodeAttemptCount(0); // Reset lại số lần nhập mã xác nhận
            accountRepository.save(account);

            return DataResponse.builder()
                    .status("success")
                    .message("Mã xác nhận mới cho đăng ký đã được gửi lại!")
                    .build();
        } else {
            throw new ErrorException("Mã xác nhận hiện tại vẫn còn hiệu lực.");
        }
    }

    @Override
    public DataResponse<Object> resendVerificationCodeForForgotPassword(ResendVerificationRequest resendRequest) {

        List<FieldErrorDetail> errors = new ArrayList<>();

        if (!isValidEmail(resendRequest.getEmailRequest())) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        AccountEntity account = accountRepository.findAccountByEmail(resendRequest.getEmailRequest());
        if (account == null) {
            errors.add(new FieldErrorDetail("emailRequest", "Email không tồn tại trong hệ thống."));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        // Kiểm tra nếu mã đã bị vô hiệu hóa do nhập sai quá nhiều lần hoặc đã hết hạn
        boolean isCodeExpired = LocalDateTime.now().isAfter(account.getVerifyCodeExpirationTime());

        if (account.getVerifyCodeAttemptCount() >= 3 || isCodeExpired) {
            String newVerifyCode = RandomUtils.getRandomVerifyCode();
            sendForgotPasswordEmail(account.getEmail(), newVerifyCode, account); // Gửi mã xác nhận mới cho quên mật khẩu

            // Cập nhật lại thông tin mã xác nhận mới
            account.setVerifyCode(newVerifyCode);
            account.setVerifyCodeExpirationTime(LocalDateTime.now().plusMinutes(5)); // 5 phút
            account.setVerifyCodeAttemptCount(0); // Reset lại số lần nhập mã xác nhận
            accountRepository.save(account);

            return DataResponse.builder()
                    .status("success")
                    .message("Mã xác nhận mới cho quên mật khẩu đã được gửi lại!")
                    .build();
        } else {
            throw new ErrorException("Mã xác nhận hiện tại vẫn còn hiệu lực.");
        }
    }

    /*
     * Thay đổi email khi đợi mã xác nhận
     */
    @Override
    public DataResponse<Object> changeEmail(ChangeEmailRequest changeEmailRequest) {
        // Tìm tài khoản dựa trên email cũ
        List<FieldErrorDetail> errors = new ArrayList<>();

        AccountEntity account = accountRepository.findAccountByEmail(changeEmailRequest.getOldEmail());

        if (changeEmailRequest.getNewEmail().equalsIgnoreCase(changeEmailRequest.getOldEmail())) {
            errors.add(new FieldErrorDetail("newEmail", "Email mới không được trùng với email cũ. Vui lòng nhập email khác."));
        }

        if (!isValidEmail(changeEmailRequest.getNewEmail())) {
            errors.add(new FieldErrorDetail("newEmail", "Email mới không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }

        AccountEntity existingAccount = accountRepository.findAccountByEmail(changeEmailRequest.getNewEmail());
        if (existingAccount != null) {
            errors.add(new FieldErrorDetail("newEmail", "Email mới đã tồn tại trong hệ thống. Vui lòng nhập email khác."));
        }

//        if (!isValidEmail(changeEmailRequest.getOldEmail())) {
//            errors.add(new FieldErrorDetail("oldEmail", "Email cũ không hợp lệ! Vui lòng nhập đúng định dạng email."));
//        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
        // Cập nhật email mới
        account.setEmail(changeEmailRequest.getNewEmail());
        account.setVerifyRegister(RandomUtils.getRandomVerifyCode());
        accountRepository.save(account);

        // Gửi mã xác nhận mới tới email mới
        sendRegistrationEmail(changeEmailRequest.getNewEmail(), account.getVerifyRegister(), account);

        return DataResponse.builder()
                .status("success")
                .message("Email đã được cập nhật và mã xác nhận mới đã được gửi.")
                .build();
    }

    /*
     * Quản Lý Người Dùng
     */
    @Override
    public DataResponse<Object> updateProfile(Integer userId, UpdateInformationRequest userUpdateRequest) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        UserInformationEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy dữ liệu"));

        if (!userEntity.getPhone().equals(userUpdateRequest.getPhone()) &&
                userRepository.existsByPhone(userUpdateRequest.getPhone())) {
            errors.add(new FieldErrorDetail("phone", "Số điện thoại đã tồn tại."));
        }

        if (!userEntity.getAccount().getEmail().equals(userUpdateRequest.getEmail()) &&
                userRepository.existsByAccount_Email(userUpdateRequest.getEmail())) {
            errors.add(new FieldErrorDetail("email", "Email đã tồn tại."));
        }

        if (!isValidPhoneNumber(userUpdateRequest.getPhone())) {
            errors.add(new FieldErrorDetail("phone", "Số điện thoại không hợp lệ! Số điện thoại chỉ có 10 số và không chứa ký tự chữ cái."));
        }

        if (!isValidGender(userUpdateRequest.getGender())) {
            errors.add(new FieldErrorDetail("gender", "Giới tính không hợp lệ, chỉ chấp nhận 'NAM' hoặc 'NỮ'."));
        }

        if (!isValidEmail(userUpdateRequest.getEmail())) {
            errors.add(new FieldErrorDetail("email", "Email không hợp lệ! Vui lòng nhập đúng định dạng email."));
        }


        userEntity.setSchoolName(userUpdateRequest.getSchoolName());
        userEntity.setFirstName(userUpdateRequest.getFirstName());
        userEntity.setLastName(userUpdateRequest.getLastName());
        userEntity.setPhone(userUpdateRequest.getPhone());
        userEntity.setAvatarUrl(userUpdateRequest.getAvatarUrl());
        userEntity.setGender(userUpdateRequest.getGender());

        if (!userEntity.getAccount().getEmail().equals(userUpdateRequest.getEmail())) {
            if (accountRepository.existsByEmail(userUpdateRequest.getEmail())) {
                errors.add(new FieldErrorDetail("email", "Email đã tồn tại."));
            }
            userEntity.getAccount().setEmail(userUpdateRequest.getEmail());
        }

        if (!userEntity.getAccount().getUsername().equals(userUpdateRequest.getUsername())) {
            if (accountRepository.existsByUsername(userUpdateRequest.getUsername())) {
                errors.add(new FieldErrorDetail("username", "Tên người dùng đã tồn tại."));
            }
            userEntity.getAccount().setUsername(userUpdateRequest.getUsername());
        }

        if (userUpdateRequest.getAddress() != null) {
            updateAddress(userEntity, userUpdateRequest.getAddress());
        }

        userRepository.save(userEntity);


        AddressDTO addressDto = AddressDTO.builder()
                .line(userEntity.getAddress().getLine())
                .provinceCode(userEntity.getAddress().getProvince().getCode())
                .districtCode(userEntity.getAddress().getDistrict().getCode())
                .wardCode(userEntity.getAddress().getWard().getCode())
                .build();

        UserInformationDTO userDto = UserInformationDTO.builder()
                .id(userEntity.getId())
                .schoolName(userEntity.getSchoolName())
                .firstName(userEntity.getFirstName())
                .lastName(userEntity.getLastName())
                .phone(userEntity.getPhone())
                .avatarUrl(userEntity.getAvatarUrl())
                .gender(userEntity.getGender())
                .email(userEntity.getAccount().getEmail())
                .username(userEntity.getAccount().getUsername())
                .address(addressDto)
                .build();
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
        return DataResponse.builder()
                .status("success")
                .message("Cập nhật hồ sơ thành công")
                .data(userDto)
                .build();
    }

    @Override
    public List<UserInformationEntity> findConsultantsByDepartmentId(Integer departmentId) {
        return userRepository.findConsultantsByDepartmentId(departmentId);
    }

    @Override
    public Optional<UserInformationEntity> findConsultantById(Integer consultantId) {
        return userRepository.findConsultantById(consultantId);
    }

    @Override
    public Integer getUserIdByEmail(String email) {
        return userRepository.getUserIdByEmail(email);
    }

    @Override
    public Optional<UserInformationEntity> findById(Integer id) {
        return userRepository.findById(id);
    }

    @Override
    public void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO) {
        ProvinceEntity provinceEntity = provinceRepository.findById(addressDTO.getProvinceCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh/thành phố với mã đã cung cấp."));

        DistrictEntity districtEntity = districtRepository.findById(addressDTO.getDistrictCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã đã cung cấp."));

        if (!districtEntity.getProvince().getCode().equals(provinceEntity.getCode())) {
            throw new ErrorException("Quận/huyện không thuộc tỉnh/thành phố đã chọn.");
        }

        WardEntity wardEntity = wardRepository.findById(addressDTO.getWardCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy phường/xã với mã đã cung cấp."));

        if (!wardEntity.getDistrict().getCode().equals(districtEntity.getCode())) {
            throw new ErrorException("Phường/xã không thuộc quận/huyện đã chọn.");
        }

        AddressEntity addressEntity = userEntity.getAddress();
        if (addressEntity == null) {
            addressEntity = new AddressEntity();
        }
        addressEntity.setLine(addressDTO.getLine());
        addressEntity.setProvince(provinceEntity);
        addressEntity.setDistrict(districtEntity);
        addressEntity.setWard(wardEntity);

        userEntity.setAddress(addressEntity);
        addressRepository.save(addressEntity);
    }


    @Override
    public List<ProvinceDTO> getAllProvinces() {
        return provinceRepository.findAll().stream()
                .map(province -> new ProvinceDTO(province.getCode(), province.getFullName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<DistrictDTO> getDistrictsByProvince(String provinceCode) {
        List<DistrictEntity> districts = districtRepository.findAll().stream()
                .filter(district -> district.getProvince().getCode().equals(provinceCode))
                .collect(Collectors.toList());

        return districts.stream()
                .map(district -> new DistrictDTO(district.getCode(), district.getFullName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<WardDTO> getWardsByDistrict(String districtCode) {
        List<WardEntity> wards = wardRepository.findAll().stream()
                .filter(ward -> ward.getDistrict().getCode().equals(districtCode))
                .collect(Collectors.toList());

        return wards.stream()
                .map(ward -> new WardDTO(ward.getCode(), ward.getFullName()))
                .collect(Collectors.toList());
    }
}