package studentConsulting.controller.advisor;

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
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorQuestionService;
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
public class AdvisorQuestionController {

    @Autowired
    private IAdvisorQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/question/list-question-by-department")
    public DataResponse<Page<MyQuestionDTO>> getDepartmentConsultantsQuestionsFilters(Principal principal,
                                                                                      @RequestParam(required = false) String title,
                                                                                      @RequestParam(required = false) String status,
                                                                                      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                                      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                                      @RequestParam(defaultValue = "0") int page,
                                                                                      @RequestParam(defaultValue = "10") int size,
                                                                                      @RequestParam(defaultValue = "createdAt") String sortBy,
                                                                                      @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(
                departmentId, title, filterStatus != null ? filterStatus.getKey() : null, startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi thành công.")
                .data(questions)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/all-deletion-log/list")
    public ResponseEntity<DataResponse<Page<DeletionLogEntity>>> getDeletionLogsByDepartment(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "deletedAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity manager = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy trưởng ban tư vấn"));

        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogEntity> logs = questionService.getDeletionLogsByDepartment(departmentId, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<DeletionLogEntity>>builder()
                        .status("success")
                        .message("Lấy lý do xóa thành công.")
                        .data(logs)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/question/detail")
    public ResponseEntity<DataResponse<MyQuestionDTO>> getQuestionByIdAndDepartment(@RequestParam("id") Integer questionId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        MyQuestionDTO questionDTO = questionService.getQuestionByIdAndDepartment(questionId, departmentId);
        if (questionDTO == null) {
            throw new ErrorException("Không tìm thấy câu hỏi");
        }

        return ResponseEntity.ok(
                DataResponse.<MyQuestionDTO>builder()
                        .status("success")
                        .message("Lấy chi tiết câu hỏi thành công.")
                        .data(questionDTO)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-question-csv")
    public void exportQuestionsToCsv(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(null, title, status, startDate, endDate, pageable);

        if (questions.isEmpty()) {
            throw new ErrorException("Không có câu hỏi nào để xuất.");
        }

        List<String> headers = List.of("ID", "Title", "Content", "Created At", "Status", "Department", "Field", "Asker Name", "Views");
        List<List<String>> data = questions.getContent().stream()
                .map(question -> List.of(
                        question.getId() != null ? question.getId().toString() : "N/A",
                        question.getTitle() != null ? question.getTitle() : "N/A",
                        question.getContent() != null ? question.getContent() : "N/A",
                        question.getCreatedAt() != null ? question.getCreatedAt().toString() : "N/A",
                        question.getQuestionFilterStatus() != null ? question.getQuestionFilterStatus().toString() : "N/A",
                        question.getDepartment().getName(),
                        question.getField().getName(),
                        question.getAskerFirstname() + " " + question.getAskerLastname(),
                        question.getViews() != null ? question.getViews().toString() : "0"
                ))
                .collect(Collectors.toList());

        String fileName = "Questions_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Questions", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-question-pdf")
    public void exportQuestionsToPdf(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(null, title, status, startDate, endDate, pageable);

        if (questions.isEmpty()) {
            throw new IOException("Không có câu hỏi nào để xuất.");
        }

        String templatePath = "/templates/question_template.html";
        String dataRows = buildQuestionDataRows(questions.getContent());

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{questions}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Questions_" + pdfService.currentDate() + ".pdf";
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

    private String buildQuestionDataRows(List<MyQuestionDTO> questions) {
        StringBuilder dataRows = new StringBuilder();

        for (MyQuestionDTO question : questions) {
            dataRows.append("<tr>")
                    .append("<td>").append(question.getId() != null ? question.getId() : "N/A").append("</td>")
                    .append("<td>").append(question.getTitle() != null ? question.getTitle() : "N/A").append("</td>")
                    .append("<td>").append(question.getContent() != null ? question.getContent() : "N/A").append("</td>")
                    .append("<td>").append(question.getCreatedAt() != null ? question.getCreatedAt().toString() : "N/A").append("</td>")
                    .append("<td>").append(question.getQuestionFilterStatus() != null ? question.getQuestionFilterStatus().toString() : "N/A").append("</td>")
                    .append("<td>").append(question.getDepartment() != null ? question.getDepartment().getName() : "N/A").append("</td>")
                    .append("<td>").append(question.getField() != null ? question.getField().getName() : "N/A").append("</td>")
                    .append("<td>").append(question.getAskerFirstname() != null ? question.getAskerFirstname() + " " + question.getAskerLastname() : "N/A").append("</td>")
                    .append("<td>").append(question.getViews() != null ? question.getViews().toString() : "0").append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-question-csv")
    public ResponseEntity<?> importQuestionsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        questionService.importQuestions(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import câu hỏi thành công.")
                .build());
    }
}
