package studentConsulting.controller.admin;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminMessageService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdminMessageController {

    @Autowired
    private IAdminMessageService messageService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/message/list")
    public ResponseEntity<DataResponse<Page<MessageDTO>>> getMessages(
            @RequestParam(required = false) Integer conversationId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MessageDTO> messages = messageService.getAllMessagesWithFilters(conversationId, pageable);

        if (messages.isEmpty()) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy tin nhắn phù hợp")
            );
        }

        return ResponseEntity.ok(
                new DataResponse<>("success", "Lấy danh sách tin nhắn thành công", messages)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/message/delete")
    public ResponseEntity<DataResponse<Void>> deleteMessage(@RequestParam Integer id) {
        try {
            messageService.deleteMessageById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Xóa tin nhắn thành công")
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy tin nhắn để xóa")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/message/detail")
    public ResponseEntity<DataResponse<MessageDTO>> getMessageById(@RequestParam Integer id) {
        try {
            MessageDTO messageDTO = messageService.getMessageById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Lấy thông tin tin nhắn thành công", messageDTO)
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy tin nhắn")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-message-csv")
    public void exportMessagesToExcel(
            @RequestParam(required = false) Integer conversationId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MessageDTO> messagePage = messageService.getAllMessagesWithFilters(conversationId, pageable);
        List<MessageDTO> messages = messagePage.getContent();

        if (messages.isEmpty()) {
            throw new Exceptions.ErrorException("Không có tin nhắn nào để xuất");
        }

        List<String> headers = List.of("Message ID", "Sender Name", "Receiver Name", "Message", "Date", "Status");
        List<List<String>> data = messages.stream()
                .map(message -> {
                    String receivers = message.getReceiver().stream()
                            .map(MessageDTO.UserInformationDTO::getName)
                            .collect(Collectors.joining(", "));

                    return List.of(
                            message.getId().toString(),
                            message.getSender().getName(),
                            receivers,
                            message.getMessage(),
                            message.getDate().toString(),
                            message.getMessageStatus().toString()
                    );
                })
                .collect(Collectors.toList());


        String fileName = "Messages_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Messages", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-message-pdf")
    public void exportMessagesToPdf(
            @RequestParam(required = false) Integer conversationId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MessageDTO> messagePage = messageService.getAllMessagesWithFilters(conversationId, pageable);
        List<MessageDTO> messages = messagePage.getContent();

        if (messages.isEmpty()) {
            throw new IOException("Không có tin nhắn nào để xuất");
        }

        String templatePath = "/templates/message_template.html";
        String dataRows = buildMessageDataRows(messages);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{messages}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Messages_" + pdfService.currentDate() + ".pdf";
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

    private String buildMessageDataRows(List<MessageDTO> messages) {
        StringBuilder dataRows = new StringBuilder();

        for (MessageDTO message : messages) {
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
        }

        return dataRows.toString();
    }

}

