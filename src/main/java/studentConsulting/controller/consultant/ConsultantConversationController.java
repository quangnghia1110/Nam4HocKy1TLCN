package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
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
import studentConsulting.model.payload.request.consultant.ApproveMemberRequest;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantConversationService;
import studentConsulting.service.interfaces.user.IUserConversationService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantConversationController {

    @Autowired
    private IConsultantConversationService conversationService;

    @Autowired
    private IUserConversationService userConversationService;

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

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/consultant/conversation/update")
    public ResponseEntity<?> updateConversationName(@RequestParam Integer conversationId, @RequestParam String newName, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = userConversationService.getDetailConversationByRole(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));
            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền cập nhật tên trong cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền cập nhật tên vì cuộc trò chuyện không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        conversationService.updateConversationName(conversationId, newName);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success").message("Cập nhật tên cuộc trò chuyện thành công.").build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/conversation/delete")
    public ResponseEntity<?> deleteConsultantConversation(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = userConversationService.getDetailConversationByRole(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));
            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền xóa cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền xóa cuộc trò chuyện này vì không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        conversationService.deleteConversation(conversationId);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success").message("Cuộc trò chuyện đã được xóa thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/conversation/approve-member")
    public ResponseEntity<DataResponse<ConversationDTO>> approveMember(@RequestParam("conversationId") Integer conversationId, @RequestBody ApproveMemberRequest request, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = userConversationService.getDetailConversationByRole(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));
            if (!isMember) {
                return new ResponseEntity<>(DataResponse.<ConversationDTO>builder()
                        .message("Bạn không có quyền thêm thành viên trong cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(DataResponse.<ConversationDTO>builder()
                        .message("Bạn không có quyền thêm thành viên vì cuộc trò chuyện không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        ConversationDTO updatedConversation = conversationService.approveMembersByEmail(conversationId, request.getEmailToApprove());

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder().status("success").message("Thành viên đã được duyệt vào nhóm.").data(updatedConversation).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/conversation/remove-member")
    public ResponseEntity<?> removeMemberFromConversation(@RequestParam Integer conversationId, @RequestParam Integer userIdToRemove, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = userConversationService.getDetailConversationByRole(conversationId);
        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));
            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder().message("Bạn không có quyền xoá thành viên trong cuộc trò chuyện này.").build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền xóa thành viên vì cuộc trò chuyện không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        conversationService.removeMemberFromConversation(conversationId, userIdToRemove);

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success").message("Xoá thành viên thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/list-member")
    public ResponseEntity<?> getConversationMembers(@RequestParam Integer conversationId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = userConversationService.getDetailConversationByRole(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));

            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền xem thành viên trong cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền xem thành viên vì cuộc trò chuyện không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        List<MemberDTO> members = conversationService.findNonConsultantMembers(conversationId);

        return ResponseEntity.ok(DataResponse.<List<MemberDTO>>builder().status("success")
                .message("Danh sách thành viên trong cuộc trò chuyện, không bao gồm tư vấn viên.")
                .data(members).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/list-users")
    public ResponseEntity<DataResponse<List<EmailDTO>>> getAllUsersWithRoleUser() {
        List<EmailDTO> users = conversationService.findAllUsersWithRoleUser();
        return ResponseEntity.ok(DataResponse.<List<EmailDTO>>builder()
                .status("success")
                .message("Danh sách người dùng có vai trò USER.")
                .data(users)
                .build());
    }

}
