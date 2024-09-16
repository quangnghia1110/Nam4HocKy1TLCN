package studentConsulting.controller;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.SecurityService;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.repository.UserRepository;
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
    @Autowired
    private SecurityService securityService;
    
    @Autowired
    private UserRepository userRepository;
    
    @PreAuthorize("hasRole('USER')")
    @PostMapping("/user/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversation(
            @RequestBody CreateConversationRequest request, Principal principal) {

        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        
        ConversationDTO createdConversation = conversationService.createConversation(request, user);

        messagingTemplate.convertAndSend("/topic/conversation/" + createdConversation.getDepartmentId(), createdConversation);

        return ResponseEntity.ok(
                DataResponse.<ConversationDTO>builder()
                        .status("success")
                        .message("Cuộc trò chuyện đã được tạo thành công.")
                        .data(createdConversation)
                        .build());
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping("/consultant/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversationByConsultant(
            @RequestBody CreateConversationRequest request, Principal principal) {

        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        
        ConversationDTO createdConversation = conversationService.createConversationByConsultant(request, user);

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
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

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
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
        

        String currentUsername = principal.getName();
        UserInformationEntity user = userService.findByUsername(currentUsername)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

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
   
    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/conversation/list")
    public ResponseEntity<DataResponse<List<ConversationDTO>>> getConsultantConversations(Principal principal) {
        String username = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        List<ConversationDTO> conversations = conversationService.findConversationsByConsultantId(consultant.getId());

        return ResponseEntity.ok(
                DataResponse.<List<ConversationDTO>>builder()
                        .status("success")
                        .message("Danh sách các cuộc trò chuyện của tư vấn viên.")
                        .data(conversations)
                        .build());
    }
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/conversation/list-detail")
    public ResponseEntity<?> getConsultantConversationById(@RequestParam Integer conversationId, Principal principal) {
        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        String currentUsername = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(currentUsername)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        if (!conversation.getConsultantName().equals(consultant.getLastName() + " " + consultant.getFirstName())) {
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



    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping("/consultant/conversation/approve-member")
    public ResponseEntity<?> approveMember(@RequestParam Integer conversationId, @RequestParam Integer userId, Principal principal) {
        ConversationDTO updatedConversation = conversationService.approveMember(conversationId, userId);
        
        String username = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
       
        if (!conversation.getUserName().equals(consultant.getLastName() + " " + consultant.getFirstName())) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền thêm thành viên trong cuộc trò chuyện này.")
                    .build(),
                HttpStatus.FORBIDDEN);
        }
        return ResponseEntity.ok(
                DataResponse.<ConversationDTO>builder()
                    .status("success")
                    .message("Thành viên đã được duyệt vào nhóm.")
                    .data(updatedConversation)
                    .build()
            );    
    }

    @PreAuthorize("hasRole('USER')")
    @DeleteMapping("/user/conversation/delete")
    public ResponseEntity<?> deleteUserConversation(@RequestParam Integer conversationId, Principal principal) {
        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
       
        if (!conversation.getUserName().equals(user.getLastName() + " " + user.getFirstName())) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền xóa cuộc trò chuyện này.")
                    .build(),
                HttpStatus.FORBIDDEN);
        }
        
        conversationService.deleteConversation(conversationId);
        
        return ResponseEntity.ok(
            DataResponse.<Void>builder()
                .status("success")
                .message("Cuộc trò chuyện đã được xóa thành công.")
                .build()
        );
    }
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @DeleteMapping("/consultant/conversation/delete")
    public ResponseEntity<?> deleteConsultantConversation(@RequestParam Integer conversationId, Principal principal) {
        String username = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);
        
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
        if (!conversation.getUserName().equals(consultant.getLastName() + " " + consultant.getFirstName())) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền xóa cuộc trò chuyện này.")
                    .build(),
                HttpStatus.FORBIDDEN);
        }

        conversationService.deleteConversation(conversationId);
        
        return ResponseEntity.ok(
            DataResponse.<Void>builder()
                .status("success")
                .message("Cuộc trò chuyện đã được xóa thành công.")
                .build()
        );
    }

    @PreAuthorize("hasRole('USER')")
    @PutMapping("/user/conversation/update")
    public ResponseEntity<?> updateUserConversation(
            @RequestParam Integer conversationId, 
            @RequestParam String newName, 
            Principal principal) {

        String username = principal.getName();
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
       
        if (!conversation.getUserName().equals(user.getLastName() + " " + user.getFirstName())) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                .message("Bạn không có quyền cập nhật cuộc trò chuyện này")
                    .build(),
                HttpStatus.FORBIDDEN);
        }

        // Cập nhật tên cuộc trò chuyện
        conversationService.updateConversationName(conversationId, newName);

        return ResponseEntity.ok(
            DataResponse.<Void>builder()
                .status("success")
                .message("Tên cuộc trò chuyện đã được cập nhật thành công.")
                .build()
        );
    }


    @PreAuthorize("hasRole('TUVANVIEN')")
    @PutMapping("/consultant/conversation/update")
    public ResponseEntity<?> updateConsultantConversation(
            @RequestParam Integer conversationId, 
            @RequestParam String newName, 
            @RequestParam(required = false) Integer userIdToRemove, 
            Principal principal) {

        String username = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (!conversation.getUserName().equals(consultant.getLastName() + " " + consultant.getFirstName())) {
            return new ResponseEntity<>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền cập nhật cuộc trò chuyện này")
                    .build(),
                HttpStatus.FORBIDDEN);
        }
        conversationService.updateConversationName(conversationId, newName);

        if (userIdToRemove != null) {
            conversationService.removeMemberFromConversation(conversationId, userIdToRemove);
        }

        return ResponseEntity.ok(
            DataResponse.<Void>builder()
                .status("success")
                .message("Cập nhật cuộc trò chuyện thành công.")
                .build()
        );
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/conversation/list-member")
    public ResponseEntity<?> getConversationMembers(
            @RequestParam Integer conversationId, Principal principal) {

        String username = principal.getName();
        UserInformationEntity consultant = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (!conversation.getConsultantName().equals(consultant.getLastName() + " " + consultant.getFirstName())) {
            return new ResponseEntity<ExceptionResponse>(
                ExceptionResponse.builder()
                    .message("Bạn không có quyền trong cuộc trò chuyện này")
                    .build(),
                HttpStatus.FORBIDDEN);
        }

        List<MemberDTO> members = conversationService.findNonConsultantMembers(conversationId);

        return ResponseEntity.ok(
            DataResponse.<List<MemberDTO>>builder()
                .status("success")
                .message("Danh sách thành viên trong cuộc trò chuyện, không bao gồm tư vấn viên.")
                .data(members)
                .build()
        );
    }



    
}
