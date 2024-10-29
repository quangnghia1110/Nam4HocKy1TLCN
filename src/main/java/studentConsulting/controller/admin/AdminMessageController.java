package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.communication.MessageDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminMessageService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

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
}

