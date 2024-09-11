package studentConsulting.controller;

import java.security.Principal;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.request.socket.ConversationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.service.IConversationService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class ConversationController {

    @Autowired
    private IConversationService conversationService;

    @Autowired
    private IUserService userService;

    @Autowired
    private SimpMessagingTemplate messagingTemplate;

    @PreAuthorize("hasRole('USER')")
    @PostMapping("/user/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversation(
            @RequestBody ConversationRequest request, Principal principal) {

        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ResourceNotFoundException("Người dùng", "username", username));

        ConversationDTO createdConversation = conversationService.createConversation(request, user);

        messagingTemplate.convertAndSend("/topic/conversation/" + createdConversation.getDepartmentId(), createdConversation);

        return ResponseEntity.ok(
                DataResponse.<ConversationDTO>builder()
                        .status("success")
                        .message("Cuộc trò chuyện đã được tạo thành công.")
                        .data(createdConversation)
                        .build());
    }

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/conversation/list")
    public ResponseEntity<DataResponse<List<ConversationDTO>>> getUserConversations(Principal principal) {
        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ResourceNotFoundException("Người dùng", "username", username));

        List<ConversationDTO> conversations = conversationService.findConversationsByUserId(user.getId());

        return ResponseEntity.ok(
                DataResponse.<List<ConversationDTO>>builder()
                        .status("success")
                        .message("Danh sách các cuộc trò chuyện của người dùng.")
                        .data(conversations)
                        .build());
    }

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/conversation/list-detail")
    public ResponseEntity<?> getConversationById(@RequestParam Integer conversationId, Principal principal) {
        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ResourceNotFoundException("Cuộc trò chuyện", "ID", conversationId);
        }

        String currentUsername = principal.getName();
        UserInformationEntity user = userService.findByUsername(currentUsername)
                .orElseThrow(() -> new ResourceNotFoundException("Người dùng", "username", currentUsername));

        String fullName = user.getLastName() + " " + user.getFirstName();

        if (!conversation.getUserName().equals(fullName) && !conversation.getConsultantName().equals(fullName)) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền truy cập cuộc trò chuyện này.")
                    .build(),
                HttpStatus.FORBIDDEN);
        }

        return ResponseEntity.ok(
            DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Thông tin cuộc trò chuyện.")
                .data(conversation)
                .build()
        );
    }
}
