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
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.CommonQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateCommonQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorCommonQuestionService;
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
public class AdvisorCommonQuestionController {

    @Autowired
    private IAdvisorCommonQuestionService commonQuestionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @GetMapping("/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestions(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(departmentId, title, startDate, endDate, pageable);

        if (commonQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<CommonQuestionDTO>>builder()
                            .status("error")
                            .message("Câu hỏi chung không tìm thấy")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/common-question/convert-to-common")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> convertToCommonQuestion(@RequestParam Integer questionId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        CommonQuestionDTO commonQuestion = isAdmin
                ? commonQuestionService.convertToCommonQuestion(questionId, principal)
                : commonQuestionService.convertToCommonQuestionByDepartment(questionId, user.getAccount().getDepartment().getId(), principal);

        if (commonQuestion == null) {
            throw new ErrorException("Không tìm thấy câu hỏi với ID: " + questionId);
        }

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Chuyển đổi câu hỏi thành công.")
                .data(commonQuestion)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/advisor-admin/common-question/update", consumes = {"multipart/form-data"})
    public DataResponse<CommonQuestionDTO> updateCommonQuestion(
            @RequestParam("commonQuestionId") Integer commonQuestionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart(value = "fileName", required = false) MultipartFile fileName,
            @RequestParam("answerTitle") String answerTitle,
            @RequestParam("answerContent") String answerContent,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        UpdateCommonQuestionRequest commonQuestionRequest = UpdateCommonQuestionRequest.builder()
                .title(title)
                .content(content)
                .fileName(fileName)
                .answerTitle(answerTitle)
                .answerContent(answerContent)
                .build();

        CommonQuestionDTO updatedCommonQuestionDTO = isAdmin
                ? commonQuestionService.updateCommonQuestion(commonQuestionId, commonQuestionRequest)
                : commonQuestionService.updateCommonQuestionByDepartment(commonQuestionId, user.getAccount().getDepartment().getId(), commonQuestionRequest);

        return DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Cập nhật câu hỏi tổng hợp thành công.")
                .data(updatedCommonQuestionDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestionsByAdvisor(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = isAdmin
                ? commonQuestionService.getAllCommonQuestionsWithFilters(title, startDate, endDate, pageable)
                : commonQuestionService.getCommonQuestionsWithAdvisorFilters(user.getAccount().getDepartment().getId(), title, startDate, endDate, pageable);

        if (commonQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<CommonQuestionDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy câu hỏi chung")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/advisor-admin/common-question/delete")
    public ResponseEntity<DataResponse<Void>> deleteCommonQuestion(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (isAdmin) {
            commonQuestionService.deleteCommonQuestion(id);
        } else {
            commonQuestionService.deleteCommonQuestionByDepartment(id, user.getAccount().getDepartment().getId());
        }

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu hỏi tổng hợp thành công.")
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/common-question/detail")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> getCommonQuestionById(@RequestParam("id") Integer questionId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        CommonQuestionDTO questionDTO = isAdmin
                ? commonQuestionService.getCommonQuestionById(questionId)
                : commonQuestionService.getCommonQuestionByIdAndDepartment(questionId, user.getAccount().getDepartment().getId());

        if (questionDTO == null) {
            throw new ErrorException("Không tìm thấy câu hỏi");
        }

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Lấy chi tiết câu hỏi thành công.")
                .data(questionDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-common-question-csv")
    public void exportCommonQuestionsToCsv(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response,
            Principal principal) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(null, title, startDate, endDate, pageable);
        List<CommonQuestionDTO> questionList = commonQuestions.getContent();

        if (questionList.isEmpty()) {
            throw new ErrorException("Không có câu hỏi tổng hợp nào để xuất.");
        }

        List<String> headers = List.of("Common Question ID", "Department Name", "Field Name", "Role Ask", "Title", "Content", "File Name", "Views",
                "Created At", "Asker Firstname", "Asker Lastname", "Answer Title", "Answer Content",
                "Answer User Email", "Answer User Firstname", "Answer User Lastname", "Answer Created At", "Created By");

        List<List<String>> dataRows = questionList.stream()
                .map(question -> List.of(
                        question.getCommonQuestionId() != null ? question.getCommonQuestionId().toString() : "N/A",
                        question.getDepartment() != null ? question.getDepartment().getName() : "N/A",
                        question.getField() != null ? question.getField().getName() : "N/A",
                        question.getRoleAsk() != null ? question.getRoleAsk().getName() : "N/A",
                        question.getTitle() != null ? question.getTitle() : "N/A",
                        question.getContent() != null ? question.getContent() : "N/A",
                        question.getFileName() != null ? question.getFileName() : "N/A",
                        question.getViews() != null ? question.getViews().toString() : "N/A",
                        question.getCreatedAt() != null ? question.getCreatedAt().toString() : "N/A",
                        question.getAskerFirstname() != null ? question.getAskerFirstname() : "N/A",
                        question.getAskerLastname() != null ? question.getAskerLastname() : "N/A",
                        question.getAnswerTitle() != null ? question.getAnswerTitle() : "N/A",
                        question.getAnswerContent() != null ? question.getAnswerContent() : "N/A",
                        question.getAnswerUserEmail() != null ? question.getAnswerUserEmail() : "N/A",
                        question.getAnswerUserFirstname() != null ? question.getAnswerUserFirstname() : "N/A",
                        question.getAnswerUserLastname() != null ? question.getAnswerUserLastname() : "N/A",
                        question.getAnswerCreatedAt() != null ? question.getAnswerCreatedAt().toString() : "N/A",
                        question.getCreatedBy() != null ? question.getCreatedBy() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Common_Questions_" + LocalDate.now() + ".csv";
        excelService.generateExcelFile("Common Questions", headers, dataRows, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-common-question-pdf")
    public void exportCommonQuestionsToPdf(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response,
            Principal principal) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(null, title, startDate, endDate, pageable);
        List<CommonQuestionDTO> questionList = commonQuestions.getContent();

        if (questionList.isEmpty()) {
            throw new IOException("Không có câu hỏi tổng hợp nào để xuất.");
        }

        String templatePath = "/templates/common_question_template.html";
        String dataRows = buildCommonQuestionDataRows(questionList);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{questions}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Common_Question_" + pdfService.currentDate() + ".pdf";
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

    private String buildCommonQuestionDataRows(List<CommonQuestionDTO> questions) {
        StringBuilder dataRows = new StringBuilder();

        for (CommonQuestionDTO question : questions) {
            dataRows.append("<tr>")
                    .append("<td>").append(question.getCommonQuestionId()).append("</td>")
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
                    .append("<td>").append(question.getCreatedBy()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-common-question-csv")
    public ResponseEntity<?> importCommonQuestionsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        commonQuestionService.importCommonQuestions(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import câu hỏi tổng hợp thành công.")
                .build());
    }

}