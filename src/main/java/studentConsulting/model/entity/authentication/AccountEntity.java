package studentConsulting.model.entity.authentication;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

import javax.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "account")
@NoArgsConstructor
@AllArgsConstructor
public class AccountEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "created_at", updatable = false)
    private LocalDate createdAt;

    @ManyToOne
    @JoinColumn(name = "department_id")
    @JsonIgnore
    private DepartmentEntity department;

    @ManyToOne
    @JoinColumn(name = "role_id")
    @JsonIgnore
    private RoleEntity role;

    @ManyToOne
    @JoinColumn(name = "role_consultant_id")
    @JsonIgnore
    private RoleConsultantEntity roleConsultant;

    @Column(name = "email", length = 50)
    private String email;

    @Column(name = "is_activity")
    private boolean isActivity;

    @Column(name = "password", length = 255)
    private String password;

    @Column(name = "username", length = 50)
    private String username;

    @Column(name = "verify_code", length = 50)
    private String verifyCode;

    @Column(name = "verify_register", length = 50)
    private String verifyRegister;

    @Column(name = "verify_code_expiration_time")
    private LocalDateTime verifyCodeExpirationTime;

    @Column(name = "verify_code_attempt_count", columnDefinition = "int default 0")
    private int verifyCodeAttemptCount;


    @OneToMany(mappedBy = "account", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private Set<UserInformationEntity> users;


}
