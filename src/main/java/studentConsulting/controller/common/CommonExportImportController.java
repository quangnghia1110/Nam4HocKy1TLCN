package studentConsulting.controller.common;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageAddressDTO;
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;
import studentConsulting.model.payload.dto.authentication.RoleDTO;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.department_field.ManageDepartmentDTO;
import studentConsulting.model.payload.dto.department_field.ManageFieldDTO;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.dto.question_answer.CommonQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.user.ManageRoleAskDTO;
import studentConsulting.model.payload.dto.user.ManageRoleConsultantDTO;
import studentConsulting.model.payload.dto.user.ManageUserDTO;
import studentConsulting.model.payload.request.ExportRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.admin.*;
import studentConsulting.service.interfaces.advisor.*;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommonExportImportController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @Autowired
    private IAdvisorAnswerService answerService;

    @Autowired
    private IAdvisorCommonQuestionService commonQuestionService;

    @Autowired
    private IAdvisorConsultationScheduleService consultationScheduleService;

    @Autowired
    private IAdvisorConversationService conversationService;

    @Autowired
    private IAdvisorForwardQuestionService forwardQuestionService;

    @Autowired
    private IAdvisorQuestionService questionService;

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
    private IAdminMessageService messageService;

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

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/import")
    public ResponseEntity<?> importCsv(@RequestParam("file") MultipartFile file, @RequestParam("importType") String importType) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);

        switch (importType) {
            case "answer":
                answerService.importAnswers(csvData);
                return buildResponse("Import câu trả lời thành công.");

            case "commonQuestion":
                commonQuestionService.importCommonQuestions(csvData);
                return buildResponse("Import câu hỏi chung thành công.");

            case "consultationSchedule":
                consultationScheduleService.importConsultationSchedules(csvData);
                return buildResponse("Import lịch tư vấn thành công.");

            case "conversation":
                conversationService.importConversations(csvData);
                return buildResponse("Import cuộc trò chuyện thành công.");

            case "forwardQuestion":
                forwardQuestionService.importForwardQuestions(csvData);
                return buildResponse("Import câu hỏi chuyển tiếp thành công.");

            case "question":
                questionService.importQuestions(csvData);
                return buildResponse("Import câu hỏi thành công.");

            case "account":
                accountService.importAccounts(csvData);
                return buildResponse("Import tài khoản thành công.");

            case "addresse":
                addressService.importAddresses(csvData);
                return buildResponse("Import địa chỉ thành công.");

            case "department":
                departmentService.importDepartments(csvData);
                return buildResponse("Import phòng ban thành công.");

            case "district":
                districtService.importDistricts(csvData);
                return buildResponse("Import quận/huyện thành công.");

            case "field":
                fieldService.importFields(csvData);
                return buildResponse("Import lĩnh vực thành công.");

            case "province":
                provinceService.importProvinces(csvData);
                return buildResponse("Import tỉnh/thành phố thành công.");

            case "roleAsk":
                roleAskService.importRoleAsks(csvData);
                return buildResponse("Import vai trò hỏi thành công.");

            case "roleConsultant":
                roleConsultantService.importRoleConsultants(csvData);
                return buildResponse("Import vai trò tư vấn thành công.");

            case "role":
                roleService.importRoles(csvData);
                return buildResponse("Import vai trò thành công.");

            case "userInformation":
                userInformationService.importUsers(csvData);
                return buildResponse("Import thông tin người dùng thành công.");

            case "ward":
                wardService.importWards(csvData);
                return buildResponse("Import phường/xã thành công.");

            default:
                return ResponseEntity.badRequest().body(DataResponse.builder()
                        .status("error")
                        .message("Loại import không hợp lệ.")
                        .build());
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/export")
    public void export(@RequestBody ExportRequest exportRequest, HttpServletResponse response, Principal principal) throws IOException, DocumentException {

        if (exportRequest.getDataType() == null || exportRequest.getExportType() == null) {
            throw new ErrorException("dataType và exportType là các trường bắt buộc.");
        }

        String dataType = exportRequest.getDataType();
        String exportType = exportRequest.getExportType();
        int page = exportRequest.getPage();
        int size = exportRequest.getSize();
        String sortBy = exportRequest.getSortBy();
        String sortDir = exportRequest.getSortDir();

        LocalDate startDate = exportRequest.getStartDate();
        LocalDate endDate = exportRequest.getEndDate();
        Integer conversationId = exportRequest.getConversationId();
        Optional<String> departId = exportRequest.getDepartId();
        String title = exportRequest.getTitle();
        Integer toDepartmentId = exportRequest.getToDepartmentId();
        String status = exportRequest.getStatus();
        String name = exportRequest.getName();
        String email = exportRequest.getEmail();
        String username = exportRequest.getUsername();
        Optional<Boolean> isOnline = exportRequest.getIsOnline();
        Optional<Boolean> isActivity = exportRequest.getIsActivity();
        Integer id = exportRequest.getId();
        String line = exportRequest.getLine();
        String provinceCode = exportRequest.getProvinceCode();
        String districtCode = exportRequest.getDistrictCode();
        String wardCode = exportRequest.getWardCode();
        Optional<String> code = exportRequest.getCode();
        Optional<String> nameEn = exportRequest.getNameEn();
        Optional<String> fullName = exportRequest.getFullName();
        Optional<String> fullNameEn = exportRequest.getFullNameEn();
        Optional<String> codeName = exportRequest.getCodeName();
        Integer roleId = exportRequest.getRoleId();
        String studentCode = exportRequest.getStudentCode();

        List<String> headers;
        List<List<String>> dataRows;
        String fileName;
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        String templatePath;
        String dataRow;
        String fileKey;

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(principal.getName());
        if (userOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();


        if("csv".equals(exportType)) {
            switch (dataType) {
                case "answer":
                    Optional<Integer> depId = isAdmin ? Optional.empty() : Optional.of(user.getAccount().getDepartment().getId());

                    Page<AnswerDTO> answers = answerService.getAllAnswersByDepartmentWithFilters(depId, startDate, endDate, page, size, sortBy, sortDir);
                    headers = List.of("Answer ID", "Question ID", "Role Consultant ID", "User ID", "Title", "Content", "File", "Created At", "Approval Status", "Answer Status");
                    dataRows = answers.getContent().stream()
                            .map(answer -> List.of(
                                    getStringValue(answer.getAnswerId()),
                                    getStringValue(answer.getQuestionId()),
                                    getStringValue(answer.getRoleConsultantId()),
                                    getStringValue(answer.getUserId()),
                                    getStringValue(answer.getTitle()),
                                    getStringValue(answer.getContent()),
                                    getStringValue(answer.getFile()),
                                    getStringValue(answer.getCreatedAt()),
                                    getStringValue(answer.getStatusApproval()),
                                    getStringValue(answer.getStatusAnswer())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Answers_" + LocalDate.now() + ".csv";
                    break;

                case "commonQuestion":
                    Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(null, title, startDate, endDate, pageable);
                    headers = List.of("Common Question ID", "Department Name", "Field Name", "Role Ask", "Title", "Content", "File Name", "Views", "Created At", "Asker Firstname", "Asker Lastname", "Answer Title", "Answer Content", "Answer User Email", "Answer User Firstname", "Answer User Lastname", "Answer Created At", "Created By");
                    dataRows = commonQuestions.getContent().stream()
                            .map(question -> List.of(
                                    getStringValue(question.getCommonQuestionId()),
                                    getStringValue(question.getDepartment().getName()),
                                    getStringValue(question.getField().getName()),
                                    getStringValue(question.getRoleAsk().getName()),
                                    getStringValue(question.getTitle()),
                                    getStringValue(question.getContent()),
                                    getStringValue(question.getFileName()),
                                    getStringValue(question.getViews()),
                                    getStringValue(question.getCreatedAt()),
                                    getStringValue(question.getAskerFirstname()),
                                    getStringValue(question.getAskerLastname()),
                                    getStringValue(question.getAnswerTitle()),
                                    getStringValue(question.getAnswerContent()),
                                    getStringValue(question.getAnswerUserEmail()),
                                    getStringValue(question.getAnswerUserFirstname()),
                                    getStringValue(question.getAnswerUserLastname()),
                                    getStringValue(question.getAnswerCreatedAt()),
                                    getStringValue(question.getCreatedBy())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Common_Questions_" + LocalDate.now() + ".csv";
                    break;

                case "consultationSchedule":
                    Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getAllConsultationSchedulesWithFilters(title, null, null, null, startDate, endDate, pageable);
                    headers = List.of("ID", "Title", "Content", "Consultant Name", "Consultation Date", "Consultation Time", "Location", "Link", "Mode", "Public", "Confirmed", "Department ID", "Created By");
                    dataRows = schedules.getContent().stream()
                            .map(schedule -> List.of(
                                    getStringValue(schedule.getId()),
                                    getStringValue(schedule.getTitle()),
                                    getStringValue(schedule.getContent()),
                                    getStringValue(schedule.getConsultantName()),
                                    getStringValue(schedule.getConsultationDate()),
                                    getStringValue(schedule.getConsultationTime()),
                                    getStringValue(schedule.getLocation()),
                                    getStringValue(schedule.getLink()),
                                    getStringValue(schedule.getMode()),
                                    getStringValue(schedule.getStatusPublic()),
                                    getStringValue(schedule.getStatusConfirmed()),
                                    getStringValue(schedule.getDepartment().getId()),
                                    getStringValue(schedule.getCreatedBy())
                            ))
                            .collect(Collectors.toList());
                    fileName = "ConsultationSchedules_" + LocalDate.now() + ".csv";
                    break;

                case "conversation":
                    Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(null, name, startDate, endDate, pageable);
                    headers = List.of("ID", "Name", "Is Group", "Created At", "Department ID");
                    dataRows = conversations.getContent().stream()
                            .map(conversation -> List.of(
                                    getStringValue(conversation.getId()),
                                    getStringValue(conversation.getName()),
                                    getStringValue(conversation.getIsGroup()),
                                    getStringValue(conversation.getCreatedAt()),
                                    getStringValue(conversation.getDepartment().getId())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Conversations_" + LocalDate.now() + ".csv";
                    break;

                case "forwardQuestion":
                    Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionByRole(title, toDepartmentId, startDate, endDate, pageable, userId, departmentId, isAdmin, isAdvisor);
                    headers = List.of("Forward Question ID", "From Department", "To Department", "Consultant", "Created By", "Status Forward");
                    dataRows = forwardQuestions.getContent().stream()
                            .map(question -> List.of(
                                    getStringValue(question.getId()),
                                    question.getFromDepartment() != null ? question.getFromDepartment().getName() : "N/A",
                                    question.getToDepartment() != null ? question.getToDepartment().getName() : "N/A",
                                    question.getConsultant() != null ? question.getConsultant().getFullName() : "N/A",
                                    getStringValue(question.getCreatedBy()),
                                    getStringValue(question.getStatusForward())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Forward_Questions_" + LocalDate.now() + ".csv";
                    break;


                case "question":
                    Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(null, title, status, startDate, endDate, pageable);
                    headers = List.of("ID", "Title", "Content", "Created At", "Status", "Department", "Field", "Asker Name", "Views");
                    dataRows = questions.getContent().stream()
                            .map(question -> List.of(
                                    getStringValue(question.getId()),
                                    getStringValue(question.getTitle()),
                                    getStringValue(question.getContent()),
                                    getStringValue(question.getCreatedAt()),
                                    getStringValue(question.getFilterStatus()),
                                    question.getDepartment().getName(),
                                    question.getField().getName(),
                                    question.getAskerFirstname() + " " + question.getAskerLastname(),
                                    getStringValue(question.getViews())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Questions_" + LocalDate.now() + ".csv";
                    break;

                case "account":
                    Page<ManageAccountDTO> accounts = accountService.getAllAccountsWithFilters(Optional.ofNullable(email), Optional.ofNullable(username), isOnline, Optional.ofNullable(startDate), Optional.ofNullable(endDate), isActivity, pageable);
                    headers = List.of("User ID", "Username", "Email", "Is Online", "Is Active", "Created At", "Last Activity", "Department", "Role", "Role Consultant");
                    dataRows = accounts.getContent().stream()
                            .map(account -> List.of(
                                    getStringValue(account.getId()),
                                    getStringValue(account.getUsername()),
                                    getStringValue(account.getEmail()),
                                    getStringValue(account.getIsOnline()),
                                    account.getIsActivity() ? "Có" : "Không",
                                    getStringValue(account.getCreatedAt()),
                                    getStringValue(account.getLastActivity()),
                                    getStringValue(account.getDepartment() != null ? account.getDepartment().getName() : "N/A"),
                                    getStringValue(account.getRole() != null ? account.getRole().getName() : "N/A"),
                                    getStringValue(account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : "N/A")
                            ))
                            .collect(Collectors.toList());
                    fileName = "Accounts_" + LocalDate.now() + ".csv";
                    break;
                case "address":
                    Page<ManageAddressDTO> addressPage = addressService.getAllAddressesWithFilters(id, line, provinceCode, districtCode, wardCode, pageable);
                    headers = List.of("Address ID", "Line", "Province Code", "District Code", "Ward Code");
                    dataRows = addressPage.getContent().stream()
                            .map(address -> List.of(
                                    getStringValue(address.getId()),
                                    getStringValue(address.getLine()),
                                    getStringValue(address.getProvinceCode()),
                                    getStringValue(address.getDistrictCode()),
                                    getStringValue(address.getWardCode())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Addresses_" + LocalDate.now() + ".csv";
                    break;
                case "department":
                    Page<ManageDepartmentDTO> departmentPage = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);
                    headers = List.of("Department ID", "Department Name", "Created At", "Description", "Logo");
                    dataRows = departmentPage.getContent().stream()
                            .map(department -> List.of(
                                    getStringValue(department.getId()),
                                    getStringValue(department.getName()),
                                    getStringValue(department.getCreatedAt()),
                                    getStringValue(department.getDescription()),
                                    getStringValue(department.getLogo())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Departments_" + LocalDate.now() + ".csv";
                    break;
                case "district":
                    Page<ManageDistrictDTO> districtPage = districtService.getAllDistrictsWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, Optional.of(provinceCode), pageable);
                    headers = List.of("District Code", "District Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name", "Province Code");
                    dataRows = districtPage.getContent().stream()
                            .map(district -> List.of(
                                    getStringValue(district.getCode()),
                                    getStringValue(district.getName()),
                                    getStringValue(district.getNameEn()),
                                    getStringValue(district.getFullName()),
                                    getStringValue(district.getFullNameEn()),
                                    getStringValue(district.getCodeName()),
                                    getStringValue(provinceCode)
                            ))
                            .collect(Collectors.toList());
                    fileName = "Districts_" + LocalDate.now() + ".csv";
                    break;
                case "field":

                    Page<ManageFieldDTO> fieldPage = fieldService.getAllFieldsWithFilters(Optional.ofNullable(name), departId, pageable);
                    headers = List.of("Field ID", "Field Name", "Created At", "DepartmentId");
                    dataRows = fieldPage.getContent().stream()
                            .map(field -> List.of(
                                    getStringValue(field.getId()),
                                    getStringValue(field.getName()),
                                    getStringValue(field.getCreatedAt()),
                                    getStringValue(field.getDepartmentId())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Fields_" + LocalDate.now() + ".csv";
                    break;
                case "message":
                    Page<MessageDTO> messagePage = messageService.getAllMessagesWithFilters(conversationId, pageable);
                    headers = List.of("Message ID", "Sender Name", "Receiver Name", "Message", "Date", "Status");
                    dataRows = messagePage.getContent().stream()
                            .map(message -> {
                                String receivers = message.getReceiver().stream()
                                        .map(MessageDTO.UserInformationDTO::getName)
                                        .collect(Collectors.joining(", "));
                                return List.of(
                                        getStringValue(message.getId()),
                                        getStringValue(message.getSender().getName()),
                                        getStringValue(receivers),
                                        getStringValue(message.getMessage()),
                                        getStringValue(message.getDate()),
                                        getStringValue(message.getMessageStatus())
                                );
                            })
                            .collect(Collectors.toList());
                    fileName = "Messages_" + LocalDate.now() + ".csv";
                    break;
                case "province":
                    Page<ManageProvinceDTO> provincePage = provinceService.getAllProvincesWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, pageable);
                    headers = List.of("Province Code", "Province Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name");
                    dataRows = provincePage.getContent().stream()
                            .map(province -> List.of(
                                    getStringValue(province.getCode()),
                                    getStringValue(province.getName()),
                                    getStringValue(province.getNameEn()),
                                    getStringValue(province.getFullName()),
                                    getStringValue(province.getFullNameEn()),
                                    getStringValue(province.getCodeName())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Provinces_" + LocalDate.now() + ".csv";
                    break;
                case "roleAsk":
                    Page<ManageRoleAskDTO> roleAskPage = roleAskService.getAllRoleAsksWithFilters(Optional.ofNullable(name), Optional.ofNullable(roleId), pageable);
                    headers = List.of("Role Ask ID", "Name", "Role ID", "Created At");
                    dataRows = roleAskPage.getContent().stream()
                            .map(roleAsk -> List.of(
                                    getStringValue(roleAsk.getId()),
                                    getStringValue(roleAsk.getName()),
                                    getStringValue(roleAsk.getRoleId()),
                                    getStringValue(roleAsk.getCreatedAt())
                            ))
                            .collect(Collectors.toList());
                    fileName = "RoleAsks_" + LocalDate.now() + ".csv";
                    break;

                case "roleConsultant":
                    Page<ManageRoleConsultantDTO> roleConsultantPage = roleConsultantService.getAllRoleConsultantsWithFilters(Optional.ofNullable(name), Optional.ofNullable(roleId), pageable);
                    headers = List.of("Role Consultant ID", "Name", "Role ID", "Created At");
                    dataRows = roleConsultantPage.getContent().stream()
                            .map(roleConsultant -> List.of(
                                    getStringValue(roleConsultant.getId()),
                                    getStringValue(roleConsultant.getName()),
                                    getStringValue(roleConsultant.getRoleId()),
                                    getStringValue(roleConsultant.getCreatedAt())
                            ))
                            .collect(Collectors.toList());
                    fileName = "RoleConsultants_" + LocalDate.now() + ".csv";
                    break;

                case "role":
                    Page<RoleDTO> rolePage = roleService.getAllRolesWithFilters(Optional.ofNullable(name), pageable);
                    headers = List.of("Role ID", "Role Name");
                    dataRows = rolePage.getContent().stream()
                            .map(role -> List.of(
                                    getStringValue(role.getId()),
                                    getStringValue(role.getName()),
                                    getStringValue(role.getCreatedAt())
                            ))
                            .collect(Collectors.toList());
                    fileName = "Roles_" + LocalDate.now() + ".csv";
                    break;

                case "userInformation":
                    Page<ManageUserDTO> userPage = userInformationService.getAllUsersWithFilters(Optional.ofNullable(name), Optional.ofNullable(studentCode), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
                    headers = List.of("User ID", "First Name", "Last Name", "Student Code", "Gender", "Phone", "Email", "Created At", "Address Line", "Province", "District", "Ward");
                    dataRows = userPage.getContent().stream()
                            .map(users -> List.of(
                                    getStringValue(users.getId()),
                                    getStringValue(users.getFirstName()),
                                    getStringValue(users.getLastName()),
                                    getStringValue(users.getStudentCode()),
                                    getStringValue(users.getGender()),
                                    getStringValue(users.getPhone()),
                                    getStringValue(users.getSchoolName()),
                                    getStringValue(users.getCreatedAt()),
                                    getStringValue(users.getAddress().getLine()),
                                    getStringValue(users.getAddress().getProvinceFullName()),
                                    getStringValue(users.getAddress().getDistrictFullName()),
                                    getStringValue(users.getAddress().getWardFullName())
                            ))
                            .collect(Collectors.toList());
                    fileName = "UserInformation_" + LocalDate.now() + ".csv";
                    break;
                case "ward":
                    Page<ManageWardDTO> wardPage = wardService.getAllWardsWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);
                    headers = List.of("Ward Code", "Ward Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name", "District Code");
                    dataRows = wardPage.getContent().stream()
                            .map(ward -> List.of(
                                    getStringValue(ward.getCode()),
                                    getStringValue(ward.getName()),
                                    getStringValue(ward.getNameEn()),
                                    getStringValue(ward.getFullName()),
                                    getStringValue(ward.getFullNameEn()),
                                    getStringValue(ward.getCodeName()),
                                    getStringValue(districtCode)
                            ))
                            .collect(Collectors.toList());
                    fileName = "Wards_" + LocalDate.now() + ".csv";
                    break;

                default:
                    throw new IllegalArgumentException("Invalid export type");
            }

            excelService.generateExcelFile(dataType, headers, dataRows, fileName, response);
        } else if("pdf".equals(exportType)) {
            switch (dataType) {
                case "answer":
                    Optional<Integer> optionalDepartmentId = isAdmin ? Optional.empty() : Optional.of(departmentId);
                    Page<AnswerDTO> answers = answerService.getAllAnswersByDepartmentWithFilters(optionalDepartmentId, startDate, endDate, page, size, sortBy, sortDir);
                    if (answers.isEmpty()) throw new IOException("Không có câu trả lời nào để xuất.");
                    templatePath = "/templates/answer_template.html";
                    dataRow = buildDataRows(answers.getContent());
                    fileName = "Answers_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{answers}}";
                    break;

                case "commonQuestion":
                    Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(departmentId, title, startDate, endDate, pageable);
                    if (commonQuestions.isEmpty()) throw new IOException("Không có câu hỏi tổng hợp nào để xuất.");
                    templatePath = "/templates/common_question_template.html";
                    dataRow = buildDataRows(commonQuestions.getContent());
                    fileName = "Common_Question_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{questions}}";
                    break;

                case "consultationSchedule":
                    Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getAllConsultationSchedulesWithFilters(title, null, null, null, startDate, endDate, pageable);
                    if (schedules.isEmpty()) throw new IOException("Không có lịch tư vấn nào để xuất.");
                    templatePath = "/templates/consultation_schedule_template.html";
                    dataRow = buildDataRows(schedules.getContent());
                    fileName = "ConsultationSchedules_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{schedules}}";
                    break;

                case "conversation":
                    Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(departmentId, name, startDate, endDate, pageable);
                    if (conversations.isEmpty()) throw new IOException("Không có cuộc trò chuyện nào để xuất.");
                    templatePath = "/templates/conversation_template.html";
                    dataRow = buildDataRows(conversations.getContent());
                    fileName = "Conversations_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{conversations}}";
                    break;

                case "forwardQuestion":
                    Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionByRole(title, toDepartmentId, startDate, endDate, pageable, userId, departmentId, isAdmin, isAdvisor);
                    if (forwardQuestions.isEmpty()) throw new IOException("Không có câu hỏi nào để xuất.");
                    templatePath = "/templates/forward_question_template.html";
                    dataRow = buildDataRows(forwardQuestions.getContent());
                    fileName = "ForwardQuestions_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{forwardQuestions}}";
                    break;

                case "question":
                    Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(departmentId, title, status, startDate, endDate, pageable);
                    if (questions.isEmpty()) throw new IOException("Không có câu hỏi nào để xuất.");
                    templatePath = "/templates/question_template.html";
                    dataRow = buildDataRows(questions.getContent());
                    fileName = "Questions_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{questions}}";
                    break;

                case "account":
                    Page<ManageAccountDTO> accounts = accountService.getAllAccountsWithFilters(Optional.ofNullable(email), Optional.ofNullable(username), isOnline, Optional.ofNullable(startDate), Optional.ofNullable(endDate), isActivity, pageable);
                    if (accounts.isEmpty()) throw new IOException("Không có tài khoản nào để xuất.");
                    templatePath = "/templates/account_template.html";
                    dataRow = buildDataRows(accounts.getContent());
                    fileName = "Accounts_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{accounts}}";
                    break;

                case "address":
                    Page<ManageAddressDTO> addresses = addressService.getAllAddressesWithFilters(id, line, provinceCode, districtCode, wardCode, pageable);
                    if (addresses.isEmpty()) throw new IOException("Không có địa chỉ nào để xuất.");
                    templatePath = "/templates/address_template.html";
                    dataRow = buildDataRows(addresses.getContent());
                    fileName = "Addresses_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{addresses}}";
                    break;

                case "department":
                    Page<ManageDepartmentDTO> departments = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);
                    if (departments.isEmpty()) throw new IOException("Không có phòng ban nào để xuất.");
                    templatePath = "/templates/department_template.html";
                    dataRow = buildDataRows(departments.getContent());
                    fileName = "Departments_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{departments}}";
                    break;

                case "district":
                    Page<ManageDistrictDTO> districts = districtService.getAllDistrictsWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, Optional.of(provinceCode), pageable);
                    if (districts.isEmpty()) throw new IOException("Không có quận/huyện nào để xuất.");
                    templatePath = "/templates/district_template.html";
                    dataRow = buildDataRows(districts.getContent());
                    fileName = "Districts_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{districts}}";
                    break;

                case "field":
                    Page<ManageFieldDTO> fields = fieldService.getAllFieldsWithFilters(Optional.ofNullable(name), departId, pageable);
                    if (fields.isEmpty()) throw new IOException("Không có lĩnh vực nào để xuất.");
                    templatePath = "/templates/field_template.html";
                    dataRow = buildDataRows(fields.getContent());
                    fileName = "Fields_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{fields}}";
                    break;

                case "message":
                    Page<MessageDTO> messages = messageService.getAllMessagesWithFilters(conversationId, pageable);
                    if (messages.isEmpty()) throw new IOException("Không có tin nhắn nào để xuất.");
                    templatePath = "/templates/message_template.html";
                    dataRow = buildDataRows(messages.getContent());
                    fileName = "Messages_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{messages}}";
                    break;

                case "province":
                    Page<ManageProvinceDTO> provinces = provinceService.getAllProvincesWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, pageable);
                    if (provinces.isEmpty()) throw new IOException("Không có tỉnh/thành nào để xuất.");
                    templatePath = "/templates/province_template.html";
                    dataRow = buildDataRows(provinces.getContent());
                    fileName = "Provinces_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{provinces}}";
                    break;

                case "roleAsk":
                    Page<ManageRoleAskDTO> roleAsks = roleAskService.getAllRoleAsksWithFilters(Optional.ofNullable(name), Optional.ofNullable(roleId), pageable);
                    if (roleAsks.isEmpty()) throw new IOException("Không có role ask nào để xuất.");
                    templatePath = "/templates/role_ask_template.html";
                    dataRow = buildDataRows(roleAsks.getContent());
                    fileName = "RoleAsks_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{role_asks}}";
                    break;

                case "userInformation":
                    Page<ManageUserDTO> users = userInformationService.getAllUsersWithFilters(Optional.ofNullable(name), Optional.ofNullable(studentCode), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
                    if (users.isEmpty()) throw new IOException("Không có người dùng nào để xuất.");
                    templatePath = "/templates/user_template.html";
                    dataRow = buildDataRows(users.getContent());
                    fileName = "UserInformation_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{users}}";
                    break;

                case "ward":
                    Page<ManageWardDTO> wards = wardService.getAllWardsWithFilters(code, Optional.ofNullable(name), nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);
                    if (wards.isEmpty()) throw new IOException("Không có Phường/Xã nào để xuất.");
                    templatePath = "/templates/ward_template.html";
                    dataRow = buildDataRows(wards.getContent());
                    fileName = "Wards_" + pdfService.currentDate() + ".pdf";
                    fileKey = "{{wards}}";
                    break;

                default:
                    throw new IllegalArgumentException("Loại xuất file lỗi");
            }

            Map<String, String> placeholders = Map.of(
                    "{{date}}", pdfService.currentDate(),
                    fileKey, dataRow,
                    "{{logo_url}}", FilePaths.LOGO_URL
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
    }

    private String getStringValue(Object obj) {
        return obj != null ? obj.toString() : "N/A";
    }

    private ResponseEntity<DataResponse> buildResponse(String message) {
        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message(message)
                .build());
    }

    private String buildDataRows(List<?> items) {
        StringBuilder dataRows = new StringBuilder();

        for (Object item : items) {
            dataRows.append("<tr>");

            if (item instanceof AnswerDTO) {
                AnswerDTO answer = (AnswerDTO) item;
                dataRows.append("<td>").append(answer.getAnswerId()).append("</td>")
                        .append("<td>").append(answer.getQuestionId()).append("</td>")
                        .append("<td>").append(answer.getRoleConsultantId()).append("</td>")
                        .append("<td>").append(answer.getUserId()).append("</td>")
                        .append("<td>").append(answer.getTitle()).append("</td>")
                        .append("<td>").append(answer.getContent()).append("</td>")
                        .append("<td>").append(answer.getFile()).append("</td>")
                        .append("<td>").append(answer.getCreatedAt()).append("</td>")
                        .append("<td>").append(answer.getStatusApproval()).append("</td>")
                        .append("<td>").append(answer.getStatusAnswer()).append("</td>");
            } else if (item instanceof CommonQuestionDTO) {
                CommonQuestionDTO question = (CommonQuestionDTO) item;
                dataRows.append("<td>").append(question.getCommonQuestionId()).append("</td>")
                        .append("<td>").append(question.getDepartment() != null ? question.getDepartment().getName() : "N/A").append("</td>")
                        .append("<td>").append(question.getField() != null ? question.getField().getName() : "N/A").append("</td>")
                        .append("<td>").append(question.getRoleAsk() != null ? question.getRoleAsk().getName() : "N/A").append("</td>")
                        .append("<td>").append(question.getTitle()).append("</td>")
                        .append("<td>").append(question.getContent()).append("</td>")
                        .append("<td>").append(question.getFileName()).append("</td>")
                        .append("<td>").append(question.getViews()).append("</td>")
                        .append("<td>").append(question.getCreatedAt()).append("</td>")
                        .append("<td>").append(question.getAskerFirstname()).append("</td>")
                        .append("<td>").append(question.getAskerLastname()).append("</td>")
                        .append("<td>").append(question.getAnswerTitle()).append("</td>")
                        .append("<td>").append(question.getAnswerContent()).append("</td>")
                        .append("<td>").append(question.getAnswerUserEmail()).append("</td>")
                        .append("<td>").append(question.getAnswerUserFirstname()).append("</td>")
                        .append("<td>").append(question.getAnswerUserLastname()).append("</td>")
                        .append("<td>").append(question.getAnswerCreatedAt()).append("</td>")
                        .append("<td>").append(question.getCreatedBy()).append("</td>");
            } else if (item instanceof ConsultationScheduleDTO) {
                ConsultationScheduleDTO schedule = (ConsultationScheduleDTO) item;
                dataRows.append("<td>").append(schedule.getId() != null ? schedule.getId() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getTitle() != null ? schedule.getTitle() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getContent() != null ? schedule.getContent() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getConsultantName() != null ? schedule.getConsultantName() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getConsultationDate() != null ? schedule.getConsultationDate() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getConsultationTime() != null ? schedule.getConsultationTime() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getLocation() != null ? schedule.getLocation() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getLink() != null ? schedule.getLink() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getMode() != null ? schedule.getMode().toString() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getStatusPublic() != null ? schedule.getStatusPublic().toString() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getStatusConfirmed() != null ? schedule.getStatusConfirmed().toString() : "N/A").append("</td>")
                        .append("<td>").append(schedule.getCreatedBy() != null ? schedule.getCreatedBy() : "N/A").append("</td>");
            } else if (item instanceof ConversationDTO) {
                ConversationDTO conversation = (ConversationDTO) item;
                dataRows.append("<td>").append(conversation.getId()).append("</td>")
                        .append("<td>").append(conversation.getName()).append("</td>")
                        .append("<td>").append(conversation.getIsGroup()).append("</td>")
                        .append("<td>").append(conversation.getCreatedAt()).append("</td>")
                        .append("<td>").append(conversation.getDepartment().getId()).append("</td>");
            } else if (item instanceof ForwardQuestionDTO) {
                ForwardQuestionDTO question = (ForwardQuestionDTO) item;
                dataRows.append("<td>").append(question.getId()).append("</td>")
                        .append("<td>").append(question.getFromDepartment().getName()).append("</td>")
                        .append("<td>").append(question.getToDepartment().getName()).append("</td>")
                        .append("<td>").append(question.getConsultant().getFullName()).append("</td>")
                        .append("<td>").append(question.getCreatedBy() != null ? question.getCreatedBy().toString() : "N/A").append("</td>")
                        .append("<td>").append(question.getStatusForward() != null ? question.getStatusForward().toString() : "N/A").append("</td>");
            } else if (item instanceof MyQuestionDTO) {
                MyQuestionDTO question = (MyQuestionDTO) item;
                dataRows.append("<td>").append(question.getId() != null ? question.getId() : "N/A").append("</td>")
                        .append("<td>").append(question.getTitle() != null ? question.getTitle() : "N/A").append("</td>")
                        .append("<td>").append(question.getContent() != null ? question.getContent() : "N/A").append("</td>")
                        .append("<td>").append(question.getCreatedAt() != null ? question.getCreatedAt().toString() : "N/A").append("</td>")
                        .append("<td>").append(question.getFilterStatus() != null ? question.getFilterStatus() : "N/A").append("</td>")
                        .append("<td>").append(question.getDepartment() != null ? question.getDepartment().getName() : "N/A").append("</td>")
                        .append("<td>").append(question.getField() != null ? question.getField().getName() : "N/A").append("</td>")
                        .append("<td>").append(question.getAskerFirstname() != null ? question.getAskerFirstname() + " " + question.getAskerLastname() : "N/A").append("</td>")
                        .append("<td>").append(question.getViews() != null ? question.getViews().toString() : "0").append("</td>");
            } else if (item instanceof ManageAccountDTO){
                ManageAccountDTO account = (ManageAccountDTO) item;
                dataRows.append("<td>").append(account.getId()).append("</td>")
                        .append("<td>").append(account.getUsername()).append("</td>")
                        .append("<td>").append(account.getEmail()).append("</td>")
                        .append("<td>").append(account.getIsOnline() != null ? account.getIsOnline() : "N/A").append("</td>")
                        .append("<td>").append(account.getIsActivity() ? "Có" : "Không").append("</td>")
                        .append("<td>").append(account.getCreatedAt()).append("</td>")
                        .append("<td>").append(account.getLastActivity() != null ? account.getLastActivity() : "N/A").append("</td>")
                        .append("<td>").append(account.getDepartment() != null ? account.getDepartment().getName() : "N/A").append("</td>")
                        .append("<td>").append(account.getRole() != null ? account.getRole().getName() : "N/A").append("</td>")
                        .append("<td>").append(account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : "N/A").append("</td>");
            } if (item instanceof ManageAddressDTO) {
                ManageAddressDTO address = (ManageAddressDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(address.getId()).append("</td>")
                        .append("<td>").append(address.getLine()).append("</td>")
                        .append("<td>").append(address.getProvinceCode()).append("</td>")
                        .append("<td>").append(address.getDistrictCode()).append("</td>")
                        .append("<td>").append(address.getWardCode()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageDepartmentDTO) {
                ManageDepartmentDTO dept = (ManageDepartmentDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(dept.getId()).append("</td>")
                        .append("<td>").append(dept.getName()).append("</td>")
                        .append("<td>").append(dept.getCreatedAt()).append("</td>")
                        .append("<td>").append(dept.getDescription()).append("</td>")
                        .append("<td>").append(dept.getLogo()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageDistrictDTO) {
                ManageDistrictDTO district = (ManageDistrictDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(district.getCode()).append("</td>")
                        .append("<td>").append(district.getName()).append("</td>")
                        .append("<td>").append(district.getNameEn()).append("</td>")
                        .append("<td>").append(district.getFullName()).append("</td>")
                        .append("<td>").append(district.getFullNameEn()).append("</td>")
                        .append("<td>").append(district.getCodeName()).append("</td>")
                        .append("<td>").append(district.getProvinceCode()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageFieldDTO) {
                ManageFieldDTO field = (ManageFieldDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(field.getId()).append("</td>")
                        .append("<td>").append(field.getName()).append("</td>")
                        .append("<td>").append(field.getCreatedAt()).append("</td>")
                        .append("<td>").append(field.getDepartmentId()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof MessageDTO) {
                MessageDTO message = (MessageDTO) item;
                String receivers = message.getReceiver().stream()
                        .map(MessageDTO.UserInformationDTO::getName)
                        .collect(Collectors.joining(", "));
                dataRows.append("<tr>")
                        .append("<td>").append(message.getId()).append("</td>")
                        .append("<td>").append(message.getSender().getName()).append("</td>")
                        .append("<td>").append(receivers).append("</td>")
                        .append("<td>").append(message.getMessage()).append("</td>")
                        .append("<td>").append(message.getDate()).append("</td>")
                        .append("<td>").append(message.getMessageStatus()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageProvinceDTO) {
                ManageProvinceDTO province = (ManageProvinceDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(province.getCode()).append("</td>")
                        .append("<td>").append(province.getName()).append("</td>")
                        .append("<td>").append(province.getNameEn()).append("</td>")
                        .append("<td>").append(province.getFullName()).append("</td>")
                        .append("<td>").append(province.getFullNameEn()).append("</td>")
                        .append("<td>").append(province.getCodeName()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageRoleAskDTO) {
                ManageRoleAskDTO roleAsk = (ManageRoleAskDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(roleAsk.getId()).append("</td>")
                        .append("<td>").append(roleAsk.getName()).append("</td>")
                        .append("<td>").append(roleAsk.getRoleId()).append("</td>")
                        .append("<td>").append(roleAsk.getCreatedAt()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageRoleConsultantDTO) {
                ManageRoleConsultantDTO roleConsultant = (ManageRoleConsultantDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(roleConsultant.getId()).append("</td>")
                        .append("<td>").append(roleConsultant.getName()).append("</td>")
                        .append("<td>").append(roleConsultant.getRoleId()).append("</td>")
                        .append("<td>").append(roleConsultant.getCreatedAt()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof RoleDTO) {
                RoleDTO role = (RoleDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(role.getId()).append("</td>")
                        .append("<td>").append(role.getName()).append("</td>")
                        .append("<td>").append(role.getCreatedAt()).append("</td>")
                        .append("</tr>");
            } else if (item instanceof Map) {
                Map<String, String> user = (Map<String, String>) item;
                dataRows.append("<tr>")
                        .append("<td>").append(user.get("userId")).append("</td>")
                        .append("<td>").append(user.get("firstName")).append("</td>")
                        .append("<td>").append(user.get("lastName")).append("</td>")
                        .append("<td>").append(user.get("studentCode")).append("</td>")
                        .append("<td>").append(user.get("gender")).append("</td>")
                        .append("<td>").append(user.get("phone")).append("</td>")
                        .append("<td>").append(user.get("email")).append("</td>")
                        .append("<td>").append(user.get("createdAt")).append("</td>")
                        .append("<td>").append(user.get("addressLine")).append("</td>")
                        .append("<td>").append(user.get("province")).append("</td>")
                        .append("<td>").append(user.get("district")).append("</td>")
                        .append("<td>").append(user.get("ward")).append("</td>")
                        .append("</tr>");
            } else if (item instanceof ManageWardDTO) {
                ManageWardDTO ward = (ManageWardDTO) item;
                dataRows.append("<tr>")
                        .append("<td>").append(ward.getCode()).append("</td>")
                        .append("<td>").append(ward.getName()).append("</td>")
                        .append("<td>").append(ward.getNameEn()).append("</td>")
                        .append("<td>").append(ward.getFullName()).append("</td>")
                        .append("<td>").append(ward.getFullNameEn()).append("</td>")
                        .append("<td>").append(ward.getCodeName()).append("</td>")
                        .append("<td>").append(ward.getDistrictCode()).append("</td>")
                        .append("</tr>");
            }

            dataRows.append("</tr>");
        }

        return dataRows.toString();
    }

}