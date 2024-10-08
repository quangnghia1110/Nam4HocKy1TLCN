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
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConversationService;
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
public class AdvisorConversationController {

    @Autowired
    private IAdvisorConversationService conversationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/conversation/list-consultant")
    public ResponseEntity<DataResponse<Page<ConversationDTO>>> getAdvisorConversations(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(
                departmentId, name, startDate, endDate, pageable);

        if (conversations.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConversationDTO>>builder().status("error")
                    .message("Không tìm thấy cuộc trò chuyện trong phòng ban.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder().status("success")
                .message("Lấy danh sách các cuộc trò chuyện thành công.").data(conversations).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/advisor-admin/conversation/update-consultant")
    public ResponseEntity<?> updateAdvisorConversation(@RequestParam Integer conversationId,
                                                       @RequestParam String newName,
                                                       @RequestParam(required = false) Integer userIdToRemove,
                                                       Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        conversationService.updateConversationName(conversationId, newName, departmentId);

        if (userIdToRemove != null) {
            conversationService.removeMemberFromConversation(conversationId, userIdToRemove, departmentId);
        }

        return ResponseEntity.ok(
                DataResponse.<Void>builder()
                        .status("success")
                        .message("Cập nhật cuộc trò chuyện thành công.")
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/advisor-admin/conversation/delete-consultant")
    public ResponseEntity<?> deleteAdvisorConversation(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        conversationService.deleteConversation(conversationId, departmentId);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success")
                .message("Cuộc trò chuyện đã được xóa thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/conversation/detail")
    public ResponseEntity<DataResponse<ConversationDTO>> getConversationByIdAndDepartment(@RequestParam("id") Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        ConversationDTO conversationDTO = conversationService.getConversationByIdAndDepartment(conversationId, departmentId);
        if (conversationDTO == null) {
            throw new ErrorException("Không tìm thấy cuộc trò chuyện");
        }

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Lấy chi tiết cuộc trò chuyện thành công.")
                .data(conversationDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-conversation-csv")
    public void exportConversationsToCsv(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(
                null, name, startDate, endDate, pageable);

        if (conversations.isEmpty()) {
            throw new IOException("Không có cuộc trò chuyện nào để xuất.");
        }

        List<String> headers = List.of("ID", "Name", "Is Group", "Created At", "Department ID");
        List<List<String>> data = conversations.getContent().stream()
                .map(conversation -> List.of(
                        conversation.getId() != null ? conversation.getId().toString() : "N/A",
                        conversation.getName() != null ? conversation.getName() : "N/A",
                        conversation.getIsGroup() != null ? conversation.getIsGroup().toString() : "N/A",
                        conversation.getCreatedAt() != null ? conversation.getCreatedAt().toString() : "N/A",
                        conversation.getDepartment() != null ? conversation.getDepartment().getId().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Conversations_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Conversations", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/export-conversation-pdf")
    public void exportConversationsToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            HttpServletResponse response) throws IOException, DocumentException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.findConversationsByDepartmentWithFilters(
                null, name, startDate, endDate, pageable);

        if (conversations.isEmpty()) {
            throw new IOException("Không có cuộc trò chuyện nào để xuất.");
        }

        String templatePath = "/templates/conversation_template.html";
        String dataRows = buildConversationDataRows(conversations.getContent());

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{conversations}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Conversations_" + pdfService.currentDate() + ".pdf";
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

    private String buildConversationDataRows(List<ConversationDTO> conversations) {
        StringBuilder dataRows = new StringBuilder();

        for (ConversationDTO conversation : conversations) {
            dataRows.append("<tr>")
                    .append("<td>").append(conversation.getId()).append("</td>")
                    .append("<td>").append(conversation.getName()).append("</td>")
                    .append("<td>").append(conversation.getIsGroup()).append("</td>")
                    .append("<td>").append(conversation.getCreatedAt()).append("</td>")
                    .append("<td>").append(conversation.getDepartment().getId()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/import-conversation-csv")
    public ResponseEntity<?> importConversationsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        conversationService.importConversations(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import cuộc trò chuyện thành công.")
                .build());
    }


}
