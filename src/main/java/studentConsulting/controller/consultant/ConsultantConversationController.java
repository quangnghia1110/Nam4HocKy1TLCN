package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.user.EmailDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantConversationService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantConversationController {

    @Autowired
    private IConsultantConversationService conversationService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping("/consultant/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversationByConsultant(
            @RequestBody CreateConversationRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO createdConversation = conversationService.createConversationByConsultant(request, user);

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder().status("success")
                .message("Cuộc trò chuyện đã được tạo thành công.").data(createdConversation).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/conversation/list")
    public ResponseEntity<DataResponse<Page<ConversationDTO>>> getConsultantConversations(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.findConversationsByConsultantWithFilters(user.getId(),
                name, startDate, endDate, pageable);


        return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder().status("success")
                .message("Lấy danh sách các cuộc trò chuyện thành công.").data(conversations).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/conversation/list-detail")
    public ResponseEntity<?> getConsultantConversationById(@RequestParam Integer conversationId, Principal principal) {
        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer id = user.getId();

        boolean isMember = conversation.getMembers().stream()
                .anyMatch(member -> member.getId().equals(id));

        if (!isMember) {
            return new ResponseEntity<>(ExceptionResponse.builder()
                    .message("Bạn không có quyền truy cập trong cuộc trò chuyện này.").build(),
                    HttpStatus.FORBIDDEN);
        }

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder().status("success")
                .message("Thông tin cuộc trò chuyện.").data(conversation).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PutMapping("/consultant/conversation/approve-member")
    public ResponseEntity<DataResponse<ConversationDTO>> approveMember(
            @RequestParam("conversationId") Integer conversationId,
            @RequestParam("emailToApprove") List<String> emailToApprove,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO conversation = conversationService.findConversationById(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        boolean isMember = conversation.getMembers().stream()
                .anyMatch(member -> member.getId().equals(user.getId()));

        if (!isMember) {
            return new ResponseEntity<>(DataResponse.<ConversationDTO>builder()
                    .message("Bạn không có quyền thêm thành viên trong cuộc trò chuyện này.").build(),
                    HttpStatus.FORBIDDEN);
        }

        ConversationDTO updatedConversation = conversationService.approveMembersByEmail(conversationId, emailToApprove);

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Thành viên đã được duyệt vào nhóm.")
                .data(updatedConversation)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @DeleteMapping("/consultant/conversation/delete")
    public ResponseEntity<?> deleteConsultantConversation(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }
        Integer id = user.getId();

        boolean isMember = conversation.getMembers().stream()
                .anyMatch(member -> member.getId().equals(id));

        if (!isMember) {
            return new ResponseEntity<>(ExceptionResponse.builder()
                    .message("Bạn không có quyền xóa thành viên trong cuộc trò chuyện này.").build(),
                    HttpStatus.FORBIDDEN);
        }

        conversationService.deleteConversation(conversationId);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success")
                .message("Cuộc trò chuyện đã được xóa thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PutMapping("/consultant/conversation/update")
    public ResponseEntity<?> updateConsultantConversation(@RequestParam Integer conversationId,
                                                          @RequestParam String newName, @RequestParam(required = false) Integer userIdToRemove, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        Integer id = user.getId();

        boolean isMember = conversation.getMembers().stream()
                .anyMatch(member -> member.getId().equals(id));

        if (!isMember) {
            return new ResponseEntity<>(ExceptionResponse.builder()
                    .message("Bạn không có quyền cập nhật trong cuộc trò chuyện này.").build(),
                    HttpStatus.FORBIDDEN);
        }
        conversationService.updateConversationName(conversationId, newName);

        if (userIdToRemove != null) {
            conversationService.removeMemberFromConversation(conversationId, userIdToRemove);
        }

        return ResponseEntity.ok(
                DataResponse.<Void>builder().status("success").message("Cập nhật cuộc trò chuyện thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/conversation/list-member")
    public ResponseEntity<?> getConversationMembers(@RequestParam Integer conversationId, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO conversation = conversationService.findConversationById(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        Integer id = user.getId();

        boolean isMember = conversation.getMembers().stream()
                .anyMatch(member -> member.getId().equals(id));

        if (!isMember) {
            return new ResponseEntity<>(ExceptionResponse.builder()
                    .message("Bạn không có quyền xem thành viên trong cuộc trò chuyện này.").build(),
                    HttpStatus.FORBIDDEN);
        }
        List<MemberDTO> members = conversationService.findNonConsultantMembers(conversationId);

        return ResponseEntity.ok(DataResponse.<List<MemberDTO>>builder().status("success")
                .message("Danh sách thành viên trong cuộc trò chuyện, không bao gồm tư vấn viên.").data(members)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/conversation/list-users")
    public ResponseEntity<DataResponse<List<EmailDTO>>> getAllUsersWithRoleUser() {
        List<EmailDTO> users = conversationService.findAllUsersWithRoleUser();
        return ResponseEntity.ok(DataResponse.<List<EmailDTO>>builder()
                .status("success")
                .message("Danh sách người dùng có vai trò USER.")
                .data(users)
                .build());
    }

}
