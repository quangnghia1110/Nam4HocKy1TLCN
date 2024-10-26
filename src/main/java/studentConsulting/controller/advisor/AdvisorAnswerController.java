package studentConsulting.controller.advisor;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorAnswerService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdvisorAnswerController {

    @Autowired
    private IAdvisorAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping(value = "/advisor-admin/answer/review", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(@ModelAttribute ReviewAnswerRequest reviewRequest,
                                                                Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();
        UserInformationEntity consultant = answer.getUser();

        if (!isAdmin && !consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời");
        }

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        QuestionEntity question = answer.getQuestion();
        UserInformationEntity questionOwner = question.getUser();

        NotificationEntity questionOwnerNotification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(questionOwner.getId())
                .content(NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()))
                .time(LocalDateTime.now())
                .notificationType(NotificationType.USER)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO questionOwnerNotificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(questionOwnerNotification.getSenderId())
                .receiverId(questionOwnerNotification.getReceiverId())
                .content(questionOwnerNotification.getContent())
                .time(questionOwnerNotification.getTime())
                .notificationType(questionOwnerNotification.getNotificationType().name())
                .status(questionOwnerNotification.getStatus().name())
                .build();

        NotificationResponseDTO questionOwnerResponseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(questionOwnerNotificationDTO)
                .build();

        notificationService.sendNotification(questionOwnerNotificationDTO);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(questionOwner.getId()), "/notification", questionOwnerResponseDTO);

        String consultantContent = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationContent.REVIEW_ANSWER_CONSULTANT.formatMessage(user.getLastName() + " " + user.getFirstName()) :
                NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName());

        NotificationType consultantNotificationType = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationType.TUVANVIEN : NotificationType.USER;

        NotificationEntity consultantNotification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(consultant.getId())
                .content(consultantContent)
                .time(LocalDateTime.now())
                .notificationType(consultantNotificationType)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO consultantNotificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(consultantNotification.getSenderId())
                .receiverId(consultantNotification.getReceiverId())
                .content(consultantNotification.getContent())
                .time(consultantNotification.getTime())
                .notificationType(consultantNotification.getNotificationType().name())
                .status(consultantNotification.getStatus().name())
                .build();

        NotificationResponseDTO consultantResponseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(consultantNotificationDTO)
                .build();

        notificationService.sendNotification(consultantNotificationDTO);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/notification", consultantResponseDTO);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Kiểm duyệt thành công")
                .data(reviewedAnswer).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/answer/update", consumes = {"multipart/form-data"})
    public DataResponse<AnswerDTO> updateAnswer(
            @RequestParam("answerId") Integer answerId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("statusApproval") Boolean statusApproval,
            @RequestParam("statusAnswer") Boolean statusAnswer,
            @RequestPart(value = "file", required = false) MultipartFile file,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        UpdateAnswerRequest answerRequest = UpdateAnswerRequest.builder()
                .title(title)
                .content(content)
                .statusApproval(statusApproval)
                .statusAnswer(statusAnswer)
                .file(file)
                .build();

        return DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Cập nhật câu trả lời thành công.")
                .data(answerService.updateAnswer(answerId, answerRequest, user))
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/answer/delete")
    public ResponseEntity<DataResponse<Void>> deleteAnswer(@RequestParam("id") Integer id, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        answerService.deleteAnswer(id, user);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu trả lời thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/answer/detail")
    public ResponseEntity<DataResponse<AnswerDTO>> getAnswerById(@RequestParam("id") Integer answerId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        AnswerDTO answerDTO = answerService.getAnswerById(answerId, user);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Lấy chi tiết câu trả lời thành công.")
                .data(answerDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-answer-csv")
    public void exportAnswersToCsv(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            HttpServletResponse response,
            Principal principal) throws IOException {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Optional<Integer> departmentId = isAdmin ? Optional.empty() : Optional.of(user.getAccount().getDepartment().getId());

        Page<AnswerDTO> answers = answerService.getAllAnswersByDepartmentWithFilters(departmentId, startDate, endDate, page, size, sortBy, sortDir);
        List<AnswerDTO> answerList = answers.getContent();

        if (answerList.isEmpty()) {
            throw new ErrorException("Không có câu trả lời nào để xuất.");
        }

        List<String> headers = List.of("Answer ID", "Question ID", "Role Consultant ID", "User ID", "Title", "Content", "File", "Created At", "Approval Status", "Answer Status");

        List<List<String>> dataRows = answerList.stream()
                .map(answer -> List.of(
                        answer.getAnswerId() != null ? answer.getAnswerId().toString() : "N/A",
                        answer.getQuestionId() != null ? answer.getQuestionId().toString() : "N/A",
                        answer.getRoleConsultantId() != null ? answer.getRoleConsultantId().toString() : "N/A",
                        answer.getUserId() != null ? answer.getUserId().toString() : "N/A",
                        answer.getTitle() != null ? answer.getTitle() : "N/A",
                        answer.getContent() != null ? answer.getContent() : "N/A",
                        answer.getFile() != null ? answer.getFile() : "N/A",
                        answer.getCreatedAt() != null ? answer.getCreatedAt().toString() : "N/A",
                        answer.getStatusApproval() != null ? answer.getStatusApproval().toString() : "N/A",
                        answer.getStatusAnswer() != null ? answer.getStatusAnswer().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Answers_" + LocalDate.now() + ".csv";
        excelService.generateExcelFile("Answers", headers, dataRows, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-answer-pdf")
    public void exportAnswersToPdf(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            HttpServletResponse response,
            Principal principal) throws DocumentException, IOException {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Optional<Integer> departmentId = isAdmin ? Optional.empty() : Optional.of(user.getAccount().getDepartment().getId());

        Page<AnswerDTO> answers = answerService.getAllAnswersByDepartmentWithFilters(departmentId, startDate, endDate, page, size, sortBy, sortDir);
        List<AnswerDTO> answerList = answers.getContent();

        if (answerList.isEmpty()) {
            throw new IOException("Không có câu trả lời nào để xuất.");
        }

        String templatePath = "/templates/answer_template.html";
        String dataRows = buildAnswerDataRows(answerList);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{answers}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Answers_" + pdfService.currentDate() + ".pdf";
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

    private String buildAnswerDataRows(List<AnswerDTO> answers) {
        StringBuilder dataRows = new StringBuilder();

        for (AnswerDTO answer : answers) {
            dataRows.append("<tr>")
                    .append("<td>").append(answer.getAnswerId()).append("</td>")
                    .append("<td>").append(answer.getQuestionId()).append("</td>")
                    .append("<td>").append(answer.getRoleConsultantId()).append("</td>")
                    .append("<td>").append(answer.getUserId()).append("</td>")
                    .append("<td>").append(answer.getTitle()).append("</td>")
                    .append("<td>").append(answer.getContent()).append("</td>")
                    .append("<td>").append(answer.getFile()).append("</td>")
                    .append("<td>").append(answer.getCreatedAt()).append("</td>")
                    .append("<td>").append(answer.getStatusApproval()).append("</td>")
                    .append("<td>").append(answer.getStatusAnswer()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-answer-csv")
    public ResponseEntity<?> importAddressesFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        answerService.importAnswers(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}