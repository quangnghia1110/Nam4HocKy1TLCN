package studentConsulting.controller.common;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.dto.manage.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.*;
import studentConsulting.service.interfaces.admin.*;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IExportImportService;
import studentConsulting.service.interfaces.common.IPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class ExportImportController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @Autowired
    private IPostService postService;

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private ICommonQuestionService commonQuestionService;

    @Autowired
    private IConsultationScheduleService consultationScheduleService;

    @Autowired
    private IConversationService conversationService;

    @Autowired
    private IForwardQuestionService forwardQuestionService;

    @Autowired
    private IQuestionService questionService;

    @Autowired
    private IRatingService ratingService;

    @Autowired
    private IAdminAccountService accountService;

    @Autowired
    private IAdminAdressService addressService;

    @Autowired
    private IAdminDepartmentService departmentService;

    @Autowired
    private IAdminDistrictService districtService;

    @Autowired
    private IAdminFieldService fieldService;

    @Autowired
    private IAdminProvinceService provinceService;

    @Autowired
    private IAdminRoleAskService roleAskService;

    @Autowired
    private IAdminRoleConsultantService roleConsultantService;

    @Autowired
    private IAdminRoleService roleService;

    @Autowired
    private IAdminUserInformationService userInformationService;

    @Autowired
    private IAdminWardService wardService;

    @Autowired
    private IExportImportService exportImportService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping(value = "/import", consumes = {"multipart/form-data"})
    public ResponseEntity<?> importCsv(
            @RequestPart(value = "file", required = false) MultipartFile file,
            @RequestParam("importType") String importType) throws IOException {

        List<List<String>> csvData = excelService.importCsv(file);

        switch (importType) {

            case "commonQuestion":
                exportImportService.importCommonQuestions(csvData);
                return exportImportService.buildResponse("Import câu hỏi chung thành công.");

            case "consultationSchedule":
                exportImportService.importManageConsultantSchedules(csvData);
                return exportImportService.buildResponse("Import lịch tư vấn thành công.");

            case "account":
                exportImportService.importAccounts(csvData);
                return exportImportService.buildResponse("Import tài khoản thành công.");

            case "address":
                exportImportService.importAddresses(csvData);
                return exportImportService.buildResponse("Import địa chỉ thành công.");

            case "department":
                exportImportService.importDepartments(csvData);
                return exportImportService.buildResponse("Import phòng ban thành công.");

            case "district":
                exportImportService.importDistricts(csvData);
                return exportImportService.buildResponse("Import quận/huyện thành công.");

            case "field":
                exportImportService.importFields(csvData);
                return exportImportService.buildResponse("Import lĩnh vực thành công.");

            case "province":
                exportImportService.importProvinces(csvData);
                return exportImportService.buildResponse("Import tỉnh/thành phố thành công.");

            case "roleAsk":
                exportImportService.importRoleAsks(csvData);
                return exportImportService.buildResponse("Import vai trò hỏi thành công.");

            case "roleConsultant":
                exportImportService.importRoleConsultants(csvData);
                return exportImportService.buildResponse("Import vai trò tư vấn thành công.");

            case "role":
                exportImportService.importRoles(csvData);
                return exportImportService.buildResponse("Import vai trò thành công.");

            case "userInformation":
                exportImportService.importUsers(csvData);
                return exportImportService.buildResponse("Import thông tin người dùng thành công.");

            case "ward":
                exportImportService.importWards(csvData);
                return exportImportService.buildResponse("Import phường/xã thành công.");

            default:
                return ResponseEntity.badRequest().body(DataResponse.builder()
                        .status("error")
                        .message("Loại import không hợp lệ.")
                        .build());
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/export")
    public void export(
            @RequestParam String dataType,
            @RequestParam String exportType,
            @RequestParam(required = false, defaultValue = "0") int page,
            @RequestParam(required = false, defaultValue = "5") int size,
            @RequestParam(required = false) String sortBy,
            @RequestParam(required = false, defaultValue = "asc") String sortDir,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Integer toDepartmentId,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String username,
            @RequestParam(required = false) Boolean isOnline,
            @RequestParam(required = false) Boolean isActivity,
            @RequestParam(required = false) Integer id,
            @RequestParam(required = false) String line,
            @RequestParam(required = false) String provinceCode,
            @RequestParam(required = false) String districtCode,
            @RequestParam(required = false) String wardCode,
            @RequestParam(required = false) String code,
            @RequestParam(required = false) String nameEn,
            @RequestParam(required = false) String fullName,
            @RequestParam(required = false) String fullNameEn,
            @RequestParam(required = false) String codeName,
            @RequestParam(required = false) Integer roleId,
            @RequestParam(required = false) Boolean statusPublic,
            @RequestParam(required = false) Boolean statusConfirmed,
            @RequestParam(required = false) Boolean mode,
            @RequestParam(required = false) Boolean type,
            @RequestParam(required = false) boolean isApproved,
            @RequestParam(required = false) String consultantName,
            @RequestParam(required = false) Boolean statusApproval,
            @RequestParam(required = false) Integer accountId,
            HttpServletResponse response,
            Principal principal) throws IOException, DocumentException {

        List<String> headers = new ArrayList<>();
        List<List<String>> dataRows = new ArrayList<>();

        Pageable pageable;
        if (sortBy == null || sortBy.isEmpty()) {
            pageable = PageRequest.of(page, size, Sort.unsorted());
        } else {
            pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        }

        String templatePath = "/templates/export_pdf.html";
        String tableHeaders = "";
        String reportTitle = "";
        String fileKey = "{{tableRows}}";
        String dataRow = "";
        String fileName = "Không có";


        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(principal.getName());
        if (userOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        String emaiUser = principal.getName();


        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        boolean isConsultant = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN);
        Integer departmentId1 = isAdmin ? null : user.getAccount().getDepartment().getId();
        Integer departmentId2 = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        String role = user.getAccount().getRole().getName();
        String fullUserName = user.getLastName() + " " + user.getFirstName();

        if ("csv".equals(exportType)) {
            switch (dataType) {
                case "commonQuestion":
                    Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionByRole(departmentId1, title, startDate, endDate, pageable);

                    headers = List.of("Mã câu hỏi chung", "Tiêu đề câu hỏi", "Nội dung câu hỏi", "Tiêu đề trả lời",
                            "Nội dung trả lời", "File đính kèm", "File trả lời", "Trạng thái",
                            "Tên phòng ban", "Tên người tạo", "Ngày tạo");

                    dataRows = commonQuestions.getContent().stream()
                            .map(question -> List.of(
                                    exportImportService.getStringValue(question.getCommonQuestionId()),
                                    exportImportService.getStringValue(question.getTitle()),
                                    exportImportService.getStringValue(question.getContent()),
                                    exportImportService.getStringValue(question.getAnswerTitle() != null ? question.getAnswerTitle() : "Không có"),
                                    exportImportService.getStringValue(question.getAnswerContent() != null ? question.getAnswerContent() : "Không có"),
                                    exportImportService.getStringValue(question.getFile() != null ? question.getFile() : "Không có"),
                                    exportImportService.getStringValue(question.getFileAnswer() != null ? question.getFileAnswer() : "Không có"),
                                    exportImportService.getStringValue(question.getStatus() != null ? question.getStatus().toString() : "Không có"),
                                    exportImportService.getStringValue(question.getDepartment() != null ? question.getDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(question.getCreatedBy()),
                                    exportImportService.getStringValue(question.getCreatedAt() != null ? question.getCreatedAt().toString() : "Không có")  // Ngày tạo
                            ))
                            .collect(Collectors.toList());

                    fileName = "Common_Questions_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;


                case "consultationSchedule":
                    Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationScheduleByRole(user, departmentId, title, type, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);
                    headers = List.of("Mã lịch", "Tên phòng ban", "Tiêu đề", "Nội dung", "Tên người tư vấn", "Ngày tư vấn", "Giờ tư vấn", "Địa điểm", "Link", "Trạng thái online", "Trạng thái công khai", "Trạng thái xác nhận");
                    dataRows = schedules.getContent().stream()
                            .map(schedule -> List.of(
                                    exportImportService.getStringValue(schedule.getId()),
                                    exportImportService.getStringValue(schedule.getDepartment() != null ? schedule.getDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(schedule.getTitle()),
                                    exportImportService.getStringValue(schedule.getContent()),
                                    exportImportService.getStringValue(schedule.getConsultantName() != null ? schedule.getConsultantName() : "Không có"),
                                    exportImportService.getStringValue(schedule.getConsultationDate()),
                                    exportImportService.getStringValue(schedule.getConsultationTime()),
                                    exportImportService.getStringValue(schedule.getLocation() != null ? schedule.getLocation() : "Không có"),
                                    exportImportService.getStringValue(schedule.getLink() != null ? schedule.getLink() : "Không có"),
                                    exportImportService.getStringValue(schedule.getMode() != null ? schedule.getMode() : "Không có"),
                                    exportImportService.getStringValue(schedule.getStatusPublic() != null ? schedule.getStatusPublic() : "Không có"),
                                    exportImportService.getStringValue(schedule.getStatusConfirmed() != null ? schedule.getStatusConfirmed() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "ConsultationSchedules_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "conversation":
                    Page<ConversationDTO> conversations = conversationService.getConversationByRole(userId, role, departmentId2, name, startDate, endDate, pageable);
                    headers = List.of("Mã cuộc hội thoại", "Tên cuộc hội thoại", "Tên phòng ban", "Ngày tạo", "Trạng thái nhóm");
                    dataRows = conversations.getContent().stream()
                            .map(conversation -> List.of(
                                    exportImportService.getStringValue(conversation.getId()),
                                    exportImportService.getStringValue(conversation.getName()),
                                    exportImportService.getStringValue(conversation.getDepartment() != null ? conversation.getDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(conversation.getCreatedAt()),
                                    exportImportService.getStringValue(conversation.getIsGroup() != null ? conversation.getIsGroup() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Conversations_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;


                case "forwardQuestion":
                    Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionByRole(title, startDate, endDate, pageable, userId, departmentId1, isAdmin, isAdvisor);
                    headers = List.of("Mã câu hỏi chuyển tiếp", "Tên phòng ban gửi", "Tên phòng ban nhận", "Tên người tư vấn");
                    dataRows = forwardQuestions.getContent().stream()
                            .map(question -> List.of(
                                    exportImportService.getStringValue(question.getId()),
                                    exportImportService.getStringValue(question.getFromDepartment() != null ? question.getFromDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(question.getToDepartment() != null ? question.getToDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(question.getConsultant() != null ? question.getConsultant().getName() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Forward_Questions_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "question":
                    Page<MyQuestionDTO> questions = questionService.getQuestionAnswerByRole(statusApproval, user, title, status, departmentId, startDate, endDate, pageable);
                    headers = List.of("Mã câu hỏi", "Tên phòng ban", "Tên lĩnh vực", "Tên người hỏi", "Tiêu đề", "Nội dung", "Tên người trả lời", "Tiêu đề trả lời", "Nội dung trả lời");
                    dataRows = questions.getContent().stream()
                            .map(question -> List.of(
                                    exportImportService.getStringValue(question.getId()),
                                    exportImportService.getStringValue(question.getDepartment() != null ? question.getDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(question.getField() != null ? question.getField().getName() : "Không có"),
                                    exportImportService.getStringValue((question.getAskerLastname() != null ? question.getAskerLastname() : "Không có") + " " + (question.getAskerFirstname() != null ? question.getAskerFirstname() : "Không có")),
                                    exportImportService.getStringValue(question.getTitle()),
                                    exportImportService.getStringValue(question.getContent()),
                                    exportImportService.getStringValue((question.getAnswerUserLastname() != null ? question.getAnswerUserLastname() : "Không có") + " " + (question.getAnswerUserFirstname() != null ? question.getAnswerUserFirstname() : "Không có")),
                                    exportImportService.getStringValue(question.getAnswerTitle() != null ? question.getAnswerTitle() : "Không có"),
                                    exportImportService.getStringValue(question.getAnswerContent() != null ? question.getAnswerContent() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Questions_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "post":
                    Page<PostDTO> posts = postService.getPostByRole(isApproved, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable, principal);
                    headers = List.of("Mã bài đăng", "Tiêu đề", "Nội dung", "Tên người dùng", "Ảnh đại diện", "Tên tệp đính kèm", "Ngày tạo", "Lượt xem", "Ẩn danh", "Đã duyệt");
                    dataRows = posts.getContent().stream()
                            .map(post -> List.of(
                                    exportImportService.getStringValue(post.getId()),
                                    exportImportService.getStringValue(post.getTitle()),
                                    exportImportService.getStringValue(post.getContent()),
                                    exportImportService.getStringValue(post.getName() != null ? post.getName() : "Không có"),
                                    exportImportService.getStringValue(post.getAvatarUrl() != null ? post.getAvatarUrl() : "Không có"),
                                    exportImportService.getStringValue(post.getFileName() != null ? post.getFileName() : "Không có"),
                                    exportImportService.getStringValue(post.getCreatedAt()),
                                    exportImportService.getStringValue(post.getViews()),
                                    exportImportService.getStringValue(post.isAnonymous() ? "Có" : "Không"),
                                    exportImportService.getStringValue(post.isApproved() ? "Đã duyệt" : "Chưa duyệt")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Posts_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "rating":
                    Page<RatingDTO> ratings = ratingService.getListRatingByRole(
                            email, departmentId, consultantName, startDate, endDate,
                            page, size, sortBy, sortDir, isAdmin, isAdvisor, isConsultant, departmentId2);

                    headers = List.of(
                            "Mã đánh giá", "Tên phòng ban", "Tên người dùng", "Tên tư vấn viên",
                            "Độ hài lòng chung", "Nhận xét chung", "Kiến thức chuyên môn", "Nhận xét",
                            "Thái độ", "Nhận xét", "Tốc độ phản hồi", "Nhận xét",
                            "Khả năng hiểu vấn đề", "Nhận xét", "Ngày gửi"
                    );

                    dataRows = ratings.getContent().stream()
                            .map(rating -> List.of(
                                    exportImportService.getStringValue(rating.getId()),
                                    exportImportService.getStringValue(
                                            rating.getDepartment() != null ? rating.getDepartment().getName() : "Không có"
                                    ),
                                    exportImportService.getStringValue(rating.getUser().getName() != null ? rating.getUser().getName() : "Không có"),
                                    exportImportService.getStringValue(rating.getConsultant().getName() != null ? rating.getConsultant().getName() : "Không có"),
                                    exportImportService.getStringValue(rating.getGeneralSatisfaction()),
                                    exportImportService.getStringValue(rating.getGeneralComment()),
                                    exportImportService.getStringValue(rating.getExpertiseKnowledge()),
                                    exportImportService.getStringValue(rating.getExpertiseComment()),
                                    exportImportService.getStringValue(rating.getAttitude()),
                                    exportImportService.getStringValue(rating.getAttitudeComment()),
                                    exportImportService.getStringValue(rating.getResponseSpeed()),
                                    exportImportService.getStringValue(rating.getResponseSpeedComment()),
                                    exportImportService.getStringValue(rating.getUnderstanding()),
                                    exportImportService.getStringValue(rating.getUnderstandingComment()),
                                    exportImportService.getStringValue(rating.getSubmittedAt())
                            ))
                            .collect(Collectors.toList());

                    fileName = "Ratings_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "account":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageAccountDTO> accounts = accountService.getAccountByAdmin(email, username, isOnline, Optional.ofNullable(startDate), Optional.ofNullable(endDate), isActivity, pageable);
                    headers = List.of("Mã tài khoản", "Tên đăng nhập", "Email", "Ngày tạo", "Tên phòng ban", "Vai trò", "Vai trò tư vấn", "Hoạt động");
                    dataRows = accounts.getContent().stream()
                            .map(account -> List.of(
                                    exportImportService.getStringValue(account.getId()),
                                    exportImportService.getStringValue(account.getUsername()),
                                    exportImportService.getStringValue(account.getEmail()),
                                    exportImportService.getStringValue(account.getCreatedAt()),
                                    exportImportService.getStringValue(account.getDepartment() != null ? account.getDepartment().getName() : "Không có"),
                                    exportImportService.getStringValue(account.getRole() != null ? account.getRole().getName() : "Không có"),
                                    exportImportService.getStringValue(account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : "Không có"),
                                    exportImportService.getStringValue(account.getIsActivity() ? "Hoạt động" : "Không hoạt động")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Accounts_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "department":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageDepartmentDTO> departmentPage = departmentService.getDepartmentByAdmin(name, pageable);
                    headers = List.of("Mã phòng ban", "Tên phòng ban", "Ngày tạo", "Mô tả", "Logo");
                    dataRows = departmentPage.getContent().stream()
                            .map(department -> List.of(
                                    exportImportService.getStringValue(department.getId()),
                                    exportImportService.getStringValue(department.getName()),
                                    exportImportService.getStringValue(department.getCreatedAt()),
                                    exportImportService.getStringValue(department.getDescription() != null ? department.getDescription() : "Không có"),
                                    exportImportService.getStringValue(department.getLogo() != null ? department.getLogo() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Departments_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "field":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageFieldDTO> fields = fieldService.getFieldByAdmin(name, departmentId, pageable);
                    headers = List.of("Mã lĩnh vực", "Tên lĩnh vực", "Ngày tạo", "Mã phòng ban");
                    dataRows = fields.getContent().stream()
                            .map(field -> List.of(
                                    exportImportService.getStringValue(field.getId()),
                                    exportImportService.getStringValue(field.getName()),
                                    exportImportService.getStringValue(field.getCreatedAt()),
                                    exportImportService.getStringValue(field.getDepartmentId() != null ? field.getDepartmentId() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Fields_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "role":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<RoleDTO> roles = roleService.getRoleByAdmin(name, pageable);
                    headers = List.of("Mã vai trò", "Tên vai trò");
                    dataRows = roles.getContent().stream()
                            .map(roless -> List.of(
                                    exportImportService.getStringValue(roless.getId()),
                                    exportImportService.getStringValue(roless.getName())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Roles_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "roleConsultant":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageRoleConsultantDTO> roleConsultants = roleConsultantService.getRoleConsultantByAdmin(name, Optional.ofNullable(roleId), pageable);
                    headers = List.of("Mã vai trò tư vấn", "Tên vai trò tư vấn", "Mã vai trò", "Ngày tạo");
                    dataRows = roleConsultants.getContent().stream()
                            .map(roleConsultant -> List.of(
                                    exportImportService.getStringValue(roleConsultant.getId()),
                                    exportImportService.getStringValue(roleConsultant.getName()),
                                    exportImportService.getStringValue(roleConsultant.getRoleId()),
                                    exportImportService.getStringValue(roleConsultant.getCreatedAt())
                            ))
                            .collect(Collectors.toList());
                    fileName = "RoleConsultants_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "roleAsk":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageRoleAskDTO> roleAsks = roleAskService.getRoleAskByAdmin(name, Optional.ofNullable(roleId), pageable);
                    headers = List.of("Mã vai trò hỏi", "Tên vai trò hỏi", "Mã vai trò", "Ngày tạo");
                    dataRows = roleAsks.getContent().stream()
                            .map(roleAsk -> List.of(
                                    exportImportService.getStringValue(roleAsk.getId()),
                                    exportImportService.getStringValue(roleAsk.getName()),
                                    exportImportService.getStringValue(roleAsk.getRoleId()),
                                    exportImportService.getStringValue(roleAsk.getCreatedAt())
                            ))
                            .collect(Collectors.toList());
                    fileName = "RoleAsks_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "userInformation":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageUserDTO> userPage = userInformationService.getUserByAdmin(accountId, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
                    headers = List.of("Mã người dùng", "Họ tên", "Mã sinh viên", "Giới tính", "Số điện thoại", "Email", "Ngày tạo", "Tên đường", "Tên tỉnh", "Tên huyện", "Tên xã");
                    dataRows = userPage.getContent().stream()
                            .map(users -> List.of(
                                    exportImportService.getStringValue(users.getId()),
                                    exportImportService.getStringValue((users.getLastName() != null ? users.getLastName() : "Không có") + " " + (users.getFirstName() != null ? users.getFirstName() : "Không có")),
                                    exportImportService.getStringValue(users.getStudentCode() != null ? users.getStudentCode() : "Không có"),
                                    exportImportService.getStringValue(users.getGender() != null ? users.getGender() : "Không có"),
                                    exportImportService.getStringValue(users.getPhone() != null ? users.getPhone() : "Không có"),
                                    exportImportService.getStringValue(users.getSchoolName() != null ? users.getSchoolName() : "Không có"),
                                    exportImportService.getStringValue(users.getCreatedAt()),
                                    exportImportService.getStringValue(users.getAddress() != null ? users.getAddress().getLine() : "Không có"),
                                    exportImportService.getStringValue(users.getAddress() != null ? users.getAddress().getProvinceFullName() : "Không có"),
                                    exportImportService.getStringValue(users.getAddress() != null ? users.getAddress().getDistrictFullName() : "Không có"),
                                    exportImportService.getStringValue(users.getAddress() != null ? users.getAddress().getWardFullName() : "Không có")
                            ))
                            .collect(Collectors.toList());
                    fileName = "UserInformation_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;


                case "address":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageAddressDTO> addresses = addressService.getAddressByAdmin(id, line, provinceCode, districtCode, wardCode, pageable);
                    headers = List.of("Mã địa chỉ", "Tên đường", "Mã tỉnh", "Mã huyện", "Mã xã");
                    dataRows = addresses.getContent().stream()
                            .map(address -> List.of(
                                    exportImportService.getStringValue(address.getId()),
                                    exportImportService.getStringValue(address.getLine()),
                                    exportImportService.getStringValue(address.getProvinceCode()),
                                    exportImportService.getStringValue(address.getDistrictCode()),
                                    exportImportService.getStringValue(address.getWardCode())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Addresses_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "province":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageProvinceDTO> provinces = provinceService.getProvinceByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, pageable);
                    headers = List.of("Mã tỉnh", "Tên tỉnh", "Tên (tiếng Anh)", "Tên đầy đủ", "Tên đầy đủ (tiếng Anh)", "Tên mã");
                    dataRows = provinces.getContent().stream()
                            .map(province -> List.of(
                                    exportImportService.getStringValue(province.getCode()),
                                    exportImportService.getStringValue(province.getName()),
                                    exportImportService.getStringValue(province.getNameEn() != null ? province.getNameEn() : "Không có"),
                                    exportImportService.getStringValue(province.getFullName() != null ? province.getFullName() : "Không có"),
                                    exportImportService.getStringValue(province.getFullNameEn() != null ? province.getFullNameEn() : "Không có"),
                                    exportImportService.getStringValue(province.getCodeName())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Provinces_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "district":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageDistrictDTO> districts = districtService.getDistrictByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, provinceCode, pageable);
                    headers = List.of("Mã huyện", "Tên huyện", "Tên (tiếng Anh)", "Tên đầy đủ", "Tên đầy đủ (tiếng Anh)", "Tên mã", "Mã tỉnh");
                    dataRows = districts.getContent().stream()
                            .map(district -> List.of(
                                    exportImportService.getStringValue(district.getCode()),
                                    exportImportService.getStringValue(district.getName()),
                                    exportImportService.getStringValue(district.getNameEn() != null ? district.getNameEn() : "Không có"),
                                    exportImportService.getStringValue(district.getFullName() != null ? district.getFullName() : "Không có"),
                                    exportImportService.getStringValue(district.getFullNameEn() != null ? district.getFullNameEn() : "Không có"),
                                    exportImportService.getStringValue(district.getCodeName()),
                                    exportImportService.getStringValue(district.getProvinceCode())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Districts_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                case "ward":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageWardDTO> wards = wardService.getWardByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, districtCode, pageable);
                    headers = List.of("Mã xã", "Tên xã", "Tên (tiếng Anh)", "Tên đầy đủ", "Tên đầy đủ (tiếng Anh)", "Tên mã", "Mã huyện");
                    dataRows = wards.getContent().stream()
                            .map(ward -> List.of(
                                    exportImportService.getStringValue(ward.getCode()),
                                    exportImportService.getStringValue(ward.getName()),
                                    exportImportService.getStringValue(ward.getNameEn() != null ? ward.getNameEn() : "Không có"),
                                    exportImportService.getStringValue(ward.getFullName() != null ? ward.getFullName() : "Không có"),
                                    exportImportService.getStringValue(ward.getFullNameEn() != null ? ward.getFullNameEn() : "Không có"),
                                    exportImportService.getStringValue(ward.getCodeName()),
                                    exportImportService.getStringValue(ward.getDistrictCode())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Wards_(" + excelService.currentDate() + ")_" + fullUserName + ".csv";
                    break;

                default:
                    throw new IllegalArgumentException("Invalid export type");
            }

            excelService.generateExcelFile(dataType, headers, dataRows, fileName, response);
        } else if ("pdf".equals(exportType)) {

            switch (dataType) {

                case "commonQuestion":
                    Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionByRole(departmentId1, title, startDate, endDate, pageable);
                    reportTitle = "Common Questions Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new CommonQuestionDTO());
                    dataRow = exportImportService.buildDataByPdf(commonQuestions.getContent());
                    fileName = "Common_Questions_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "consultationSchedule":
                    Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationScheduleByRole(user, departmentId, title, type, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);
                    reportTitle = "Consultation Schedules Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ConsultationScheduleDTO());
                    dataRow = exportImportService.buildDataByPdf(schedules.getContent());
                    fileName = "ConsultationSchedules_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "conversation":
                    Page<ConversationDTO> conversations = conversationService.getConversationByRole(userId, role, departmentId2, name, startDate, endDate, pageable);
                    reportTitle = "Conversations Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ConversationDTO());
                    dataRow = exportImportService.buildDataByPdf(conversations.getContent());
                    fileName = "Conversations_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "forwardQuestion":
                    Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionByRole(title, startDate, endDate, pageable, userId, departmentId1, isAdmin, isAdvisor);
                    reportTitle = "Forward Questions Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ForwardQuestionDTO());
                    dataRow = exportImportService.buildDataByPdf(forwardQuestions.getContent());
                    fileName = "ForwardQuestions_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "question":
                    Page<MyQuestionDTO> questions = questionService.getQuestionAnswerByRole(statusApproval,user, title, status, departmentId, startDate, endDate, pageable);
                    reportTitle = "Questions Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new MyQuestionDTO());
                    dataRow = exportImportService.buildDataByPdf(questions.getContent());
                    fileName = "Questions_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "post":
                    Page<PostDTO> post = postService.getPostByRole(isApproved, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable, principal);
                    reportTitle = "Posts Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new PostDTO());
                    dataRow = exportImportService.buildDataByPdf(post.getContent());
                    fileName = "Posts_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "rating":
                    Page<RatingDTO> rating = ratingService.getListRatingByRole(emaiUser, departmentId, consultantName, startDate, endDate,
                            page, size, sortBy, sortDir, isAdmin, isAdvisor, isConsultant, departmentId2);
                    reportTitle = "Ratings Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new RatingDTO());
                    dataRow = exportImportService.buildDataByPdf(rating.getContent());
                    fileName = "Ratings_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "account":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageAccountDTO> accounts = accountService.getAccountByAdmin(email, username, isOnline, Optional.ofNullable(startDate), Optional.ofNullable(endDate), isActivity, pageable);
                    reportTitle = "Accounts Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageAccountDTO());
                    dataRow = exportImportService.buildDataByPdf(accounts.getContent());
                    fileName = "Accounts_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "address":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageAddressDTO> addresses = addressService.getAddressByAdmin(id, line, provinceCode, districtCode, wardCode, pageable);
                    reportTitle = "Addresses Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageAddressDTO());
                    dataRow = exportImportService.buildDataByPdf(addresses.getContent());
                    fileName = "Addresses_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "department":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageDepartmentDTO> departments = departmentService.getDepartmentByAdmin(name, pageable);
                    reportTitle = "Departments Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageDepartmentDTO());
                    dataRow = exportImportService.buildDataByPdf(departments.getContent());
                    fileName = "Departments_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";

                    break;

                case "district":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageDistrictDTO> districts = districtService.getDistrictByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, provinceCode, pageable);
                    reportTitle = "Districts Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageDistrictDTO());
                    dataRow = exportImportService.buildDataByPdf(districts.getContent());
                    fileName = "Districts_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";

                    break;

                case "field":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageFieldDTO> fields = fieldService.getFieldByAdmin(name, departmentId, pageable);
                    reportTitle = "Fields Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageFieldDTO());
                    dataRow = exportImportService.buildDataByPdf(fields.getContent());
                    fileName = "Fields_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";

                    break;

                case "province":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageProvinceDTO> provinces = provinceService.getProvinceByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, pageable);
                    reportTitle = "Provinces Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageProvinceDTO());
                    dataRow = exportImportService.buildDataByPdf(provinces.getContent());
                    fileName = "Provinces_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "roleAsk":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageRoleAskDTO> roleAsks = roleAskService.getRoleAskByAdmin(name, Optional.ofNullable(roleId), pageable);
                    reportTitle = "Role Asks Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageRoleAskDTO());
                    dataRow = exportImportService.buildDataByPdf(roleAsks.getContent());
                    fileName = "RoleAsks_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";

                    break;

                case "roleConsultant":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageRoleConsultantDTO> roleConsultants = roleConsultantService.getRoleConsultantByAdmin(name, Optional.ofNullable(roleId), pageable);
                    reportTitle = "Role Consultants Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageRoleConsultantDTO());
                    dataRow = exportImportService.buildDataByPdf(roleConsultants.getContent());
                    fileName = "RoleConsultants_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "userInformation":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageUserDTO> users = userInformationService.getUserByAdmin(accountId, Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
                    reportTitle = "User Information Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageUserDTO());
                    dataRow = exportImportService.buildDataByPdf(users.getContent());
                    fileName = "UserInformation_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                case "ward":
                    if (!isAdmin) {
                        throw new ErrorException("Bạn không có quyền export bảng dữ liệu này");
                    }
                    Page<ManageWardDTO> wards = wardService.getWardByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, districtCode, pageable);
                    reportTitle = "Wards Report";
                    tableHeaders = exportImportService.buildHeaderByPdf(new ManageWardDTO());
                    dataRow = exportImportService.buildDataByPdf(wards.getContent());
                    fileName = "Wards_(" + pdfService.currentDate() + ")_" + fullUserName + ".pdf";
                    break;

                default:
                    throw new IllegalArgumentException("Loại xuất file lỗi");
            }

            Map<String, String> placeholders = Map.of(
                    "{{date}}", pdfService.currentDate(),
                    fileKey, dataRow,
                    "{{tableHeaders}}", tableHeaders,
                    "{{reportTitle}}", reportTitle,
                    "{{logo_url}}", FilePaths.LOGO_URL,
                    "{{name}}", fullUserName
            );

            String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;
            try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
                pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
            } catch (IOException | DocumentException e) {
                throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
            }

            try (OutputStream responseStream = response.getOutputStream()) {
                pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
                response.flushBuffer();
            } catch (IOException | DocumentException e) {
                throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
            }
        }

        if ("csv".equalsIgnoreCase(exportType)) {
            response.setContentType("text/csv; charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment; filename=\"Không có" + fileName + ".csv\"Không có");
            response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
            excelService.generateExcelFile(dataType, headers, dataRows, fileName, response);
        } else if ("pdf".equalsIgnoreCase(exportType)) {
            response.setContentType("application/pdf");
            response.setHeader("Content-Disposition", "attachment; filename=\"Không có" + fileName + ".pdf\"Không có");
            response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
            try (OutputStream outputStream = response.getOutputStream()) {
                pdfService.generatePdfFromTemplate("/templates/export_pdf.html", Map.of(), outputStream);
                outputStream.flush();
            }
        }



    }
}