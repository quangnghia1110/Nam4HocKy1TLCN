package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Data
@Builder
@Entity
@Table(name = "account")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(exclude = {"department", "role", "roleConsultant", "userInformation"})  // Tránh vòng lặp
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

    @Column(name = "last_activity")
    private LocalDateTime lastActivity;

    @Column(name = "is_online")
    private Boolean isOnline;

    @OneToOne(mappedBy = "account", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private UserInformationEntity userInformation;

    public String getName() {
        if (userInformation != null) {
            return this.userInformation.getLastName() + " " + this.userInformation.getFirstName();
        }
        return "No Name";
    }

    public String getPhone() {
        if (userInformation != null) {
            return this.userInformation.getPhone();
        }
        return "No phone";
    }

    public boolean isActivity() {
        return isActivity;
    }

    public void setActivity(boolean isActivity) {
        this.isActivity = isActivity;
    }


    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        AccountEntity that = (AccountEntity) o;

        return Objects.equals(id, that.id);
    }
}


