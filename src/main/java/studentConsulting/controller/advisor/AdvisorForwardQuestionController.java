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
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorForwardQuestionService;
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
public class AdvisorForwardQuestionController {

    @Autowired
    private IAdvisorForwardQuestionService forwardQuestionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/forward-question/list")
    public ResponseEntity<DataResponse<Page<ForwardQuestionDTO>>> getForwardQuestions(
            @RequestParam(required = false) Integer toDepartmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionsWithFilters(toDepartmentId, startDate, endDate, pageable, departmentId);

        if (forwardQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ForwardQuestionDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy câu hỏi chuyển tiếp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ForwardQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách câu hỏi chuyển tiếp thành công")
                        .data(forwardQuestions)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/advisor-admin/forward-question/update")
    public DataResponse<ForwardQuestionDTO> updateForwardQuestion(
            @RequestParam Integer forwardQuestionId,
            @RequestBody UpdateForwardQuestionRequest forwardQuestionRequest,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        ForwardQuestionDTO updatedForwardQuestion = forwardQuestionService.updateForwardQuestion(forwardQuestionId, forwardQuestionRequest, departmentId);

        return DataResponse.<ForwardQuestionDTO>builder()
                .status("success")
                .message("Cập nhật câu hỏi chuyển tiếp thành công.")
                .data(updatedForwardQuestion)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/advisor-admin/forward-question/delete")
    public DataResponse<Void> deleteForwardQuestion(
            @RequestParam Integer forwardQuestionId,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        forwardQuestionService.deleteForwardQuestion(forwardQuestionId, departmentId);

        return DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu hỏi chuyển tiếp thành công.")
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/forward-question/detail")
    public ResponseEntity<DataResponse<ForwardQuestionDTO>> getForwardQuestionByIdAndDepartment(
            @RequestParam("id") Integer forwardQuestionId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        ForwardQuestionDTO forwardQuestionDTO = forwardQuestionService.getForwardQuestionByIdAndDepartment(forwardQuestionId, departmentId);
        if (forwardQuestionDTO == null) {
            throw new ErrorException("Không tìm thấy câu hỏi");
        }

        return ResponseEntity.ok(
                DataResponse.<ForwardQuestionDTO>builder()
                        .status("success")
                        .message("Lấy chi tiết câu hỏi chuyển tiếp thành công.")
                        .data(forwardQuestionDTO)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-forward-question-csv")
    public void exportForwardQuestionsToCsv(
            @RequestParam(required = false) Integer toDepartmentId,
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

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionsWithFilters(toDepartmentId, startDate, endDate, pageable, departmentId);
        List<ForwardQuestionDTO> questions = forwardQuestions.getContent();

        if (questions.isEmpty()) {
            throw new ErrorException("Không có câu hỏi nào để xuất.");
        }

        List<String> headers = List.of("Forward Question ID", "From Department", "To Department", "Consultant", "Created By", "Status Forward");
        List<List<String>> data = questions.stream()
                .map(question -> List.of(
                        question.getId() != null ? question.getId().toString() : "N/A",
                        question.getFromDepartment().getName(),
                        question.getToDepartment().getName(),
                        question.getConsultant().getFullName(),
                        question.getCreatedBy() != null ? question.getCreatedBy().toString() : "N/A",
                        question.getStatusForward() != null ? question.getStatusForward().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Forward_Questions_" + LocalDate.now() + ".csv";
        excelService.generateExcelFile("ForwardQuestions", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-forward-question-pdf")
    public void exportForwardQuestionsToPdf(
            @RequestParam(required = false) Integer toDepartmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            HttpServletResponse response,
            Principal principal) throws IOException, DocumentException {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardQuestions = forwardQuestionService.getForwardQuestionsWithFilters(toDepartmentId, startDate, endDate, pageable, departmentId);
        List<ForwardQuestionDTO> questions = forwardQuestions.getContent();

        if (questions.isEmpty()) {
            throw new IOException("Không có câu hỏi nào để xuất.");
        }

        String templatePath = "/templates/forward_question_template.html";
        String dataRows = buildForwardQuestionDataRows(questions);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{forwardQuestions}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "ForwardQuestions_" + pdfService.currentDate() + ".pdf";
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

    private String buildForwardQuestionDataRows(List<ForwardQuestionDTO> questions) {
        StringBuilder dataRows = new StringBuilder();

        for (ForwardQuestionDTO question : questions) {
            dataRows.append("<tr>")
                    .append("<td>").append(question.getId()).append("</td>")
                    .append("<td>").append(question.getFromDepartment().getName()).append("</td>")
                    .append("<td>").append(question.getToDepartment().getName()).append("</td>")
                    .append("<td>").append(question.getConsultant().getFullName()).append("</td>")
                    .append("<td>").append(question.getCreatedBy() != null ? question.getCreatedBy().toString() : "N/A").append("</td>")
                    .append("<td>").append(question.getStatusForward() != null ? question.getStatusForward().toString() : "N/A").append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-forward-question-csv")
    public ResponseEntity<?> importForwardQuestionsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);

        forwardQuestionService.importForwardQuestions(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import câu hỏi chuyển tiếp thành công.")
                .build());
    }

}
