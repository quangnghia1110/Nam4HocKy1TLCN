package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IAnswerService;
import studentConsulting.repository.UserRepository;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

import java.util.Optional;

@RestController
@RequestMapping("/api/v1/answer")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @PostMapping(value = "/create", consumes = { "multipart/form-data" })
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart("file") MultipartFile file,
            @RequestParam("statusApproval") Boolean statusApproval) {

        // Lấy username của người dùng hiện tại
        String username = getCurrentUsername();

        // Lấy thông tin người dùng dựa trên username
        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            return ResponseEntity.badRequest().body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("User not found.")
                    .build()
            );
        }

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant(); 

        // Tạo request DTO
        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder()
                .questionId(questionId)
                .title(title)
                .content(content)
                .file(file)
                .statusApproval(statusApproval)
                .roleConsultantId(roleConsultant.getId()) 
                .consultantId(user.getId()) 
                .build();

        // Gọi service để xử lý câu trả lời
        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);

        // Trả về phản hồi API
        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Answer created successfully.")
                .data(answerDTO)
                .build());
    }

    // Hàm để lấy username của người dùng hiện tại từ SecurityContextHolder
    private String getCurrentUsername() {
        Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (principal instanceof UserDetails) {
            return ((UserDetails) principal).getUsername();
        } else {
            return principal.toString();
        }
    }
}
