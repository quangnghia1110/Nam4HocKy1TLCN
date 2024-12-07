package studentConsulting.controller.actor;

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
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.ConversationDTO;
import studentConsulting.model.payload.dto.actor.EmailDTO;
import studentConsulting.model.payload.dto.actor.MemberDTO;
import studentConsulting.model.payload.request.ApproveMemberRequest;
import studentConsulting.model.payload.request.CreateConversationRequest;
import studentConsulting.model.payload.request.CreateConversationUserRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.payload.response.ExceptionResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.IConversationService;
import studentConsulting.service.interfaces.common.INotificationService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConversationController {

    @Autowired
    private IConversationService conversationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private INotificationService notificationService;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/conversation/create")
    public ResponseEntity<DataResponse<ConversationDTO>> createConversation(
            @RequestBody CreateConversationUserRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConversationDTO createdConversation = conversationService.createConversation(request, user);

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder().status("success")
                .message("Cuộc trò chuyện đã được tạo thành công.").data(createdConversation).build());
    }

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

        Optional<UserInformationEntity> advisorOpt = userRepository.findByRoleAndDepartment(
                SecurityConstants.Role.TRUONGBANTUVAN, user.getAccount().getDepartment().getId());

        advisorOpt.ifPresent(headOfDepartment -> {
            notificationService.sendUserNotification(
                    user.getId(),
                    headOfDepartment.getId(),
                    NotificationContent.NEW_CONVERSATION_CREATED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.TRUONGBANTUVAN
            );
        });

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .message("Cuộc trò chuyện đã được tạo thành công.")
                .data(createdConversation)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/list")
    public ResponseEntity<DataResponse<Page<ConversationDTO>>> getConversationByRole(
            Principal principal,
            @RequestParam(required = false) String name,
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

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer userId = user.getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConversationDTO> conversations = conversationService.getConversationByRole(userId, role, depId, name, startDate, endDate, pageable);

        Page<ConversationDTO> filteredConversations = conversations.map(conversation -> {
            if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
                boolean isMember = conversation.getMembers().stream()
                        .anyMatch(member -> member.getId().equals(userId));

                if (!isMember) {
//                    throw new ErrorException("Bạn không có quyền truy cập vào cuộc trò chuyện này.");
                }
            } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
                if (!conversation.getDepartment().getId().equals(depId)) {
                    throw new ErrorException("Bạn không có quyền truy cập vào cuộc trò chuyện này vì không thuộc phòng ban của bạn.");
                }
            }
            return conversation;
        });

        return ResponseEntity.ok(DataResponse.<Page<ConversationDTO>>builder()
                .status("success")
                .message("Lấy danh sách cuộc trò chuyện thành công.")
                .data(filteredConversations)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/conversation/update")
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

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);
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

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);
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

        boolean isDeleted = conversationService.recordDeletion(conversationId, userId);

        if (isDeleted) {
            conversationService.deleteConversation(conversationId);
        }

        return ResponseEntity.ok(DataResponse.<Void>builder().status("success").message("Cuộc trò chuyện đã được xóa thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/conversation/detail")
    public ResponseEntity<?> getDetailConversationByRole(@RequestParam Integer conversationId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer userId = user.getId();
        String role = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);

        if (conversation == null) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        if (SecurityConstants.Role.USER.equals(role) || SecurityConstants.Role.TUVANVIEN.equals(role)) {
            boolean isMember = conversation.getMembers().stream()
                    .anyMatch(member -> member.getId().equals(userId));

            if (!isMember) {
                return new ResponseEntity<>(ExceptionResponse.builder()
//                        .message("Bạn không có quyền truy cập vào cuộc trò chuyện này.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (!conversation.getDepartment().getId().equals(depId)) {
                return new ResponseEntity<>(ExceptionResponse.builder()
                        .message("Bạn không có quyền truy cập vào cuộc trò chuyện này vì không thuộc phòng ban của bạn.")
                        .build(), HttpStatus.FORBIDDEN);
            }
        }

        return ResponseEntity.ok(DataResponse.<ConversationDTO>builder()
                .status("success")
                .data(conversation)
                .build());
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

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);
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

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);
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

        ConversationDTO conversation = conversationService.getDetailConversationByRole(conversationId);

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
        List<EmailDTO> users = conversationService.findAllUsers();
        return ResponseEntity.ok(DataResponse.<List<EmailDTO>>builder()
                .status("success")
                .message("Danh sách người dùng.")
                .data(users)
                .build());
    }
}
